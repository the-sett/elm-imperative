module Procedure exposing
    ( Procedure
    , andThen
    , break
    , catch
    , collect
    , do
    , endWith
    , fetch
    , fetchResult
    , fromTask
    , map
    , map2
    , map3
    , mapError
    , provide
    , run
    , try
    )

import Procedure.Internal as Internal exposing (Msg(..))
import Task exposing (Task)


type alias Procedure e a =
    Internal.Procedure e a


fetch : ((a -> Msg) -> Cmd Msg) -> Procedure e a
fetch generator =
    (\_ _ tagger ->
        (Ok >> tagger) |> generator
    )
        |> Internal.Procedure


fetchResult : ((Result e a -> Msg) -> Cmd Msg) -> Procedure e a
fetchResult generator =
    (\_ _ tagger ->
        generator tagger
    )
        |> Internal.Procedure


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
        |> Internal.Procedure


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
        |> Internal.Procedure


provide : a -> Procedure e a
provide =
    Task.succeed >> fromTask


fromTask : Task e a -> Procedure e a
fromTask task =
    (\_ _ resultTagger ->
        Task.attempt resultTagger task
    )
        |> Internal.Procedure


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
    (\_ _ _ -> Cmd.none) |> Internal.Procedure


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
next (Internal.Procedure procedure) resultMapper =
    (\procId msgTagger tagger ->
        (\aResult ->
            let
                (Internal.Procedure nextProcedure) =
                    resultMapper aResult
            in
            nextProcedure procId msgTagger tagger
                |> (Execute procId >> msgTagger)
        )
            |> procedure procId msgTagger
    )
        |> Internal.Procedure


try : (Msg -> Msg) -> (Result e a -> Msg) -> Procedure e a -> Cmd Msg
try msgTagger tagger (Internal.Procedure procedure) =
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
