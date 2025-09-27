module Proc exposing (..)

{-| Proc provides a structure that combines 3 things; `Result`, `Procedure` and the
state monad. This helps you do stateful programming with IO and error handling.

@docs Proc


# Imperative programs

@docs Program, program


# Constructors

@docs pure, err, result, task, advance


# Combinators for building imperative programs

@docs andThen, andMap, onError, map


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


type Msg msg
    = Initiate (Int -> Cmd msg)
    | Execute Int (Cmd msg)
    | Subscribe Int (Int -> msg) (Int -> Sub msg)
    | Unsubscribe Int Int msg
    | Continue


type Procedure e a msg
    = Procedure (Int -> (Msg msg -> msg) -> (Result e a -> msg) -> Cmd msg)



-- Procedure


fetch : ((a -> msg) -> Cmd msg) -> Procedure e a msg
fetch generator =
    (\_ _ tagger ->
        (Ok >> tagger) |> generator
    )
        |> Procedure


fetchResult : ((Result e a -> msg) -> Cmd msg) -> Procedure e a msg
fetchResult generator =
    (\_ _ tagger ->
        generator tagger
    )
        |> Procedure


do : Cmd msg -> Procedure Never () msg
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
        |> Procedure


endWith : Cmd msg -> Procedure Never Never msg
endWith command =
    (\procId msgTagger _ ->
        Task.succeed ()
            |> Task.perform
                (\_ ->
                    Execute procId command
                        |> msgTagger
                )
    )
        |> Procedure


provide : a -> Procedure e a msg
provide =
    Task.succeed >> fromTask


fromTask : Task e a -> Procedure e a msg
fromTask task =
    (\_ _ resultTagger ->
        Task.attempt resultTagger task
    )
        |> Procedure


break : e -> Procedure e a msg
break =
    Task.fail >> fromTask


catch : (e -> Procedure f a msg) -> Procedure e a msg -> Procedure f a msg
catch generator procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                provide aData

            Err eData ->
                generator eData
    )
        |> next procedure


andThen : (a -> Procedure e b msg) -> Procedure e a msg -> Procedure e b msg
andThen generator procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                generator aData

            Err eData ->
                break eData
    )
        |> next procedure


collect : List (Procedure e a msg) -> Procedure e (List a) msg
collect procedures =
    case procedures of
        [] ->
            emptyProcedure

        procedure :: remainingProcedures ->
            List.foldl (addToList >> andThen) (addToList procedure []) remainingProcedures


addToList : Procedure e a msg -> List a -> Procedure e (List a) msg
addToList procedure collector =
    (\aResult ->
        case aResult of
            Ok aData ->
                [ aData ]
                    |> List.append collector
                    |> provide

            Err eData ->
                break eData
    )
        |> next procedure


emptyProcedure : Procedure e a msg
emptyProcedure =
    (\_ _ _ -> Cmd.none) |> Procedure


map : (a -> b) -> Procedure e a msg -> Procedure e b msg
map mapper =
    andThen (mapper >> provide)


map2 : (a -> b -> c) -> Procedure e a msg -> Procedure e b msg -> Procedure e c msg
map2 mapper procedureA procedureB =
    procedureA
        |> andThen
            (\aData ->
                procedureB
                    |> map (mapper aData)
            )


map3 : (a -> b -> c -> d) -> Procedure e a msg -> Procedure e b msg -> Procedure e c msg -> Procedure e d msg
map3 mapper procedureA procedureB procedureC =
    procedureA
        |> andThen
            (\aData ->
                map2 (mapper aData) procedureB procedureC
            )


mapError : (e -> f) -> Procedure e a msg -> Procedure f a msg
mapError mapper procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                provide aData

            Err eData ->
                mapper eData
                    |> break
    )
        |> next procedure


next : Procedure e a msg -> (Result e a -> Procedure f b msg) -> Procedure f b msg
next (Procedure procedure) resultMapper =
    (\procId msgTagger tagger ->
        (\aResult ->
            let
                (Procedure nextProcedure) =
                    resultMapper aResult
            in
            nextProcedure procId msgTagger tagger
                |> (Execute procId >> msgTagger)
        )
            |> procedure procId msgTagger
    )
        |> Procedure


try : (Msg msg -> msg) -> (Result e a -> msg) -> Procedure e a msg -> Cmd msg
try msgTagger tagger (Procedure procedure) =
    Task.succeed (\procId -> procedure procId msgTagger tagger)
        |> Task.perform (Initiate >> msgTagger)


run : (Msg msg -> msg) -> (a -> msg) -> Procedure Never a msg -> Cmd msg
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


acceptOne : Channel a msg -> Procedure e a msg
acceptOne =
    always True |> acceptUntil


accept : Channel a msg -> Procedure e a msg
accept =
    always False |> acceptUntil


acceptUntil : (a -> Bool) -> Channel a msg -> Procedure e a msg
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


defaultRequest : String -> Cmd msg
defaultRequest _ =
    Cmd.none


defaultPredicate : String -> a -> Bool
defaultPredicate _ _ =
    True



-- Program


type Model msg
    = Model (Registry msg)


type alias Registry msg =
    { nextId : Int
    , channels : Dict Int (Sub msg)
    }


init : Model msg
init =
    { nextId = 0
    , channels = Dict.empty
    }
        |> Model


update : Msg msg -> Model msg -> ( Model msg, Cmd msg )
update msg (Model registry) =
    updateProcedures msg registry
        |> Tuple.mapFirst Model


updateProcedures : Msg msg -> Registry msg -> ( Registry msg, Cmd msg )
updateProcedures msg registry =
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


subscriptions : Model msg -> Sub msg
subscriptions (Model registry) =
    Dict.values registry.channels
        |> Sub.batch
