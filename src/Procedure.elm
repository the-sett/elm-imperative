module Procedure exposing (..)

import Dict exposing (Dict)
import Task exposing (Task)



-- Internal


type Msg
    = Initiate (Int -> Cmd Msg)
    | Execute Int (Cmd Msg)
    | Subscribe Int (Int -> Msg) (Int -> Sub Msg)
    | Unsubscribe Int Int Msg
    | Continue


type Procedure e a
    = Procedure (Int -> (Msg -> Msg) -> (Result e a -> Msg) -> Cmd Msg)



-- Procedure


fetch : ((a -> Msg) -> Cmd Msg) -> Procedure e a
fetch generator =
    (\_ _ tagger ->
        (Ok >> tagger) |> generator
    )
        |> Procedure


fetchResult : ((Result e a -> Msg) -> Cmd Msg) -> Procedure e a
fetchResult generator =
    (\_ _ tagger ->
        generator tagger
    )
        |> Procedure


do : Cmd Msg -> Procedure Never ()
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


endWith : Cmd Msg -> Procedure Never Never
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


provide : a -> Procedure e a
provide =
    Task.succeed >> fromTask


fromTask : Task e a -> Procedure e a
fromTask task =
    (\_ _ resultTagger ->
        Task.attempt resultTagger task
    )
        |> Procedure


break : e -> Procedure e a
break =
    Task.fail >> fromTask


catch : (e -> Procedure f a) -> Procedure e a -> Procedure f a
catch generator procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                provide aData

            Err eData ->
                generator eData
    )
        |> next procedure


andThen : (a -> Procedure e b) -> Procedure e a -> Procedure e b
andThen generator procedure =
    (\aResult ->
        case aResult of
            Ok aData ->
                generator aData

            Err eData ->
                break eData
    )
        |> next procedure


collect : List (Procedure e a) -> Procedure e (List a)
collect procedures =
    case procedures of
        [] ->
            emptyProcedure

        procedure :: remainingProcedures ->
            List.foldl (addToList >> andThen) (addToList procedure []) remainingProcedures


addToList : Procedure e a -> List a -> Procedure e (List a)
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


emptyProcedure : Procedure e a
emptyProcedure =
    (\_ _ _ -> Cmd.none) |> Procedure


map : (a -> b) -> Procedure e a -> Procedure e b
map mapper =
    andThen (mapper >> provide)


map2 : (a -> b -> c) -> Procedure e a -> Procedure e b -> Procedure e c
map2 mapper procedureA procedureB =
    procedureA
        |> andThen
            (\aData ->
                procedureB
                    |> map (mapper aData)
            )


map3 : (a -> b -> c -> d) -> Procedure e a -> Procedure e b -> Procedure e c -> Procedure e d
map3 mapper procedureA procedureB procedureC =
    procedureA
        |> andThen
            (\aData ->
                map2 (mapper aData) procedureB procedureC
            )


mapError : (e -> f) -> Procedure e a -> Procedure f a
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


next : Procedure e a -> (Result e a -> Procedure f b) -> Procedure f b
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


try : (Msg -> Msg) -> (Result e a -> Msg) -> Procedure e a -> Cmd Msg
try msgTagger tagger (Procedure procedure) =
    Task.succeed (\procId -> procedure procId msgTagger tagger)
        |> Task.perform (Initiate >> msgTagger)


run : (Msg -> Msg) -> (a -> Msg) -> Procedure Never a -> Cmd Msg
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



-- Program


type Model
    = Model Registry


type alias Registry =
    { nextId : Int
    , channels : Dict Int (Sub Msg)
    }


init : Model
init =
    { nextId = 0
    , channels = Dict.empty
    }
        |> Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model registry) =
    updateProcedures msg registry
        |> Tuple.mapFirst Model


updateProcedures : Msg -> Registry -> ( Registry, Cmd Msg )
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


addChannel : (Int -> Sub Msg) -> Registry -> Registry
addChannel subGenerator registry =
    { registry
        | nextId = registry.nextId + 1
        , channels = Dict.insert registry.nextId (subGenerator registry.nextId) registry.channels
    }


deleteChannel : Int -> Registry -> Registry
deleteChannel channelId procModel =
    { procModel | channels = Dict.remove channelId procModel.channels }


sendMessage : Msg -> Cmd Msg
sendMessage msg =
    Task.succeed ()
        |> Task.perform (always msg)


subscriptions : Model -> Sub Msg
subscriptions (Model registry) =
    Dict.values registry.channels
        |> Sub.batch
