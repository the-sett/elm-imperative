module Proc exposing (..)

{-| Proc provides a structure that combines 3 things; `Result`, `Procedure` and the
state monad. This helps you do stateful programming with IO and error handling.

@docs Proc


# Imperative programs

@docs Program, program


# Constructors

@docs pure, err, result, task, advance


# Combinators for building imperative programs

@docs andThen, andMap, onError, map, mapError


# More maps

@docs map2, map3, map4, map5, map6


# State management

@docs get, put, modify


# Sequencing lists of imperatives

@docs sequence

-}

import Dict exposing (Dict)
import Task exposing (Task)



-- Internal


type alias Registry msg =
    { nextId : Int
    , channels : Dict Int (Sub msg)
    }


type Msg msg
    = Initiate (Int -> Cmd msg)
    | Execute Int (Cmd msg)
    | Subscribe Int (Int -> msg) (Int -> Sub msg)
    | Unsubscribe Int Int msg
    | Continue


type Proc e a msg
    = Proc (Int -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg)



-- Procedure


fetch : ((a -> msg) -> Cmd msg) -> Proc e a msg
fetch generator =
    (\_ _ tagger ->
        (Ok >> tagger) |> generator
    )
        |> Proc


fetchResult : ((Result e a -> msg) -> Cmd msg) -> Proc e a msg
fetchResult generator =
    (\_ _ tagger ->
        generator tagger
    )
        |> Proc


do : Cmd msg -> Proc Never () msg
do command =
    (\procId msgTagger resultTagger ->
        Task.succeed ()
            |> Task.perform
                (\_ ->
                    let
                        nextCommand =
                            Task.succeed ()
                                |> Task.perform (Ok >> resultTagger)
                    in
                    Cmd.batch [ command, nextCommand ]
                        |> Execute procId
                        |> msgTagger
                )
    )
        |> Proc


endWith : Cmd msg -> Proc Never Never msg
endWith command =
    (\procId msgTagger _ ->
        Task.succeed ()
            |> Task.perform
                (\_ ->
                    Execute procId command
                        |> msgTagger
                )
    )
        |> Proc


pure : a -> Proc e a msg
pure =
    Task.succeed >> task


task : Task e a -> Proc e a msg
task tsk =
    (\_ _ resultTagger ->
        Task.attempt resultTagger tsk
    )
        |> Proc


err : e -> Proc e a msg
err =
    Task.fail >> task


catch : (e -> Proc f a msg) -> Proc e a msg -> Proc f a msg
catch generator procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                pure aData

            Err eData ->
                generator eData
    )
        |> next procedure


andThen : (a -> Proc e b msg) -> Proc e a msg -> Proc e b msg
andThen generator procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                generator aData

            Err eData ->
                err eData
    )
        |> next procedure


sequence : List (Proc e a msg) -> Proc e (List a) msg
sequence procedures =
    case procedures of
        [] ->
            emptyProcedure

        procedure :: remainingProcedures ->
            List.foldl (addToList >> andThen) (addToList procedure []) remainingProcedures


addToList : Proc e a msg -> List a -> Proc e (List a) msg
addToList procedure collector =
    (\aResult ->
        case aResult of
            Ok aData ->
                [ aData ]
                    |> List.append collector
                    |> pure

            Err eData ->
                err eData
    )
        |> next procedure


emptyProcedure : Proc e a msg
emptyProcedure =
    (\_ _ _ -> Cmd.none) |> Proc


map : (a -> b) -> Proc e a msg -> Proc e b msg
map mapper =
    andThen (mapper >> pure)


map2 : (a -> b -> c) -> Proc e a msg -> Proc e b msg -> Proc e c msg
map2 mapper procedureA procedureB =
    procedureA
        |> andThen
            (\aData ->
                procedureB
                    |> map (mapper aData)
            )


map3 : (a -> b -> c -> d) -> Proc e a msg -> Proc e b msg -> Proc e c msg -> Proc e d msg
map3 mapper procedureA procedureB procedureC =
    procedureA
        |> andThen
            (\aData ->
                map2 (mapper aData) procedureB procedureC
            )


mapError : (e -> f) -> Proc e a msg -> Proc f a msg
mapError mapper procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                pure aData

            Err eData ->
                mapper eData
                    |> err
    )
        |> next procedure


next : Proc e a msg -> (Result e a -> Proc f b msg) -> Proc f b msg
next (Proc procedure) resultMapper =
    (\procId msgTagger tagger ->
        (\aResult ->
            let
                (Proc nextProcedure) =
                    resultMapper aResult
            in
            nextProcedure procId msgTagger tagger
                |> (Execute procId >> msgTagger)
        )
            |> procedure procId msgTagger
    )
        |> Proc


try : (Msg msg -> msg) -> (Result e a -> msg) -> Proc e a msg -> Cmd msg
try msgTagger tagger (Proc procedure) =
    Task.succeed (\procId -> procedure procId msgTagger tagger)
        |> Task.perform (Initiate >> msgTagger)


run : (Msg msg -> msg) -> (a -> msg) -> Proc Never a msg -> Cmd msg
run msgTagger tagger =
    try msgTagger
        (\result ->
            case result of
                Ok data ->
                    tagger data

                Err e ->
                    never e
        )



-- Channel


type Channel a msg
    = Channel
        { request : String -> Cmd msg
        , subscription : (a -> msg) -> Sub msg
        , shouldAccept : String -> a -> Bool
        }


type ChannelRequest msg
    = ChannelRequest (String -> Cmd msg)


open : (String -> Cmd msg) -> ChannelRequest msg
open =
    ChannelRequest


connect : ((a -> msg) -> Sub msg) -> ChannelRequest msg -> Channel a msg
connect generator (ChannelRequest requestGenerator) =
    Channel
        { request = requestGenerator
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


join : ((a -> msg) -> Sub msg) -> Channel a msg
join generator =
    Channel
        { request = defaultRequest
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


filter : (String -> a -> Bool) -> Channel a msg -> Channel a msg
filter predicate (Channel channel) =
    Channel
        { channel | shouldAccept = predicate }


acceptOne : Channel a msg -> Proc e a msg
acceptOne =
    always True |> acceptUntil


accept : Channel a msg -> Proc e a msg
accept =
    always False |> acceptUntil


acceptUntil : (a -> Bool) -> Channel a msg -> Proc e a msg
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
        |> Proc


channelKey : Int -> String
channelKey channelId =
    "Channel-" ++ String.fromInt channelId


defaultRequest : String -> Cmd msg
defaultRequest _ =
    Cmd.none


defaultPredicate : String -> a -> Bool
defaultPredicate _ _ =
    True



-- Program


init : Registry msg
init =
    { nextId = 0
    , channels = Dict.empty
    }


update : Msg msg -> Registry msg -> ( Registry msg, Cmd msg )
update msg registry =
    case msg of
        Initiate generator ->
            ( { registry | nextId = registry.nextId + 1 }
            , generator registry.nextId
            )

        Execute _ cmd ->
            ( registry
            , cmd
            )

        Subscribe _ messageGenerator subGenerator ->
            ( addChannel subGenerator registry
            , messageGenerator registry.nextId
                |> sendMessage
            )

        Unsubscribe _ channelId nextMessage ->
            ( deleteChannel channelId registry
            , sendMessage nextMessage
            )

        Continue ->
            ( registry, Cmd.none )


addChannel : (Int -> Sub msg) -> Registry msg -> Registry msg
addChannel subGenerator registry =
    { registry
        | nextId = registry.nextId + 1
        , channels = Dict.insert registry.nextId (subGenerator registry.nextId) registry.channels
    }


deleteChannel : Int -> Registry msg -> Registry msg
deleteChannel channelId procModel =
    { procModel | channels = Dict.remove channelId procModel.channels }


sendMessage : msg -> Cmd msg
sendMessage msg =
    Task.succeed ()
        |> Task.perform (always msg)


subscriptions : Registry msg -> Sub msg
subscriptions registry =
    Dict.values registry.channels
        |> Sub.batch
