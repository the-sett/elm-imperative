module Procedure.Channel exposing
    ( Channel
    , ChannelRequest
    , accept
    , acceptOne
    , acceptUntil
    , connect
    , filter
    , join
    , open
    )

import Procedure.Internal exposing (Msg(..), Procedure(..))
import Task


type Channel a
    = Channel
        { request : String -> Cmd Msg
        , subscription : (a -> Msg) -> Sub Msg
        , shouldAccept : String -> a -> Bool
        }


type ChannelRequest
    = ChannelRequest (String -> Cmd Msg)


open : (String -> Cmd Msg) -> ChannelRequest
open =
    ChannelRequest


connect : ((a -> Msg) -> Sub Msg) -> ChannelRequest -> Channel a
connect generator (ChannelRequest requestGenerator) =
    Channel
        { request = requestGenerator
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


join : ((a -> Msg) -> Sub Msg) -> Channel a
join generator =
    Channel
        { request = defaultRequest
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


filter : (String -> a -> Bool) -> Channel a -> Channel a
filter predicate (Channel channel) =
    Channel
        { channel | shouldAccept = predicate }


acceptOne : Channel a -> Procedure e a
acceptOne =
    always True |> acceptUntil


accept : Channel a -> Procedure e a
accept =
    always False |> acceptUntil


acceptUntil : (a -> Bool) -> Channel a -> Procedure e a
acceptUntil shouldUnsubscribe (Channel channel) =
    (\procId msgTagger resultTagger ->
        let
            requestCommandMsg channelId =
                channel.request (channelKey channelId)
                    |> (Execute procId >> msgTagger)

            subGenerator channelId =
                (\aData ->
                    if channel.shouldAccept (channelKey channelId) aData then
                        generateMsg channelId aData

                    else
                        msgTagger Continue
                )
                    |> channel.subscription

            generateMsg channelId aData =
                if shouldUnsubscribe aData then
                    Ok aData
                        |> resultTagger
                        |> (Unsubscribe procId channelId >> msgTagger)

                else
                    Ok aData
                        |> resultTagger
        in
        Task.succeed subGenerator
            |> Task.perform (Subscribe procId requestCommandMsg >> msgTagger)
    )
        |> Procedure


channelKey : Int -> String
channelKey channelId =
    "Channel-" ++ String.fromInt channelId


defaultRequest : String -> Cmd Msg
defaultRequest _ =
    Cmd.none


defaultPredicate : String -> a -> Bool
defaultPredicate _ _ =
    True
