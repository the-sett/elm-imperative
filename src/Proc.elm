module Proc exposing
    ( Proc
    , Program, program
    , pure, err, result, task, advance
    , andThen, andMap, onError, map
    , map2, map3, map4, map5, map6
    , get, put, modify
    , sequence
    )

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



-- The imperative structure


type alias PRegistry s =
    { nextId : Int
    , channels : Dict Int (Sub Msg)
    , state : s
    }


{-| Proc combines `Result`, `Task` and the state monad together.
-}
type Proc s x a
    = State (PRegistry s -> ( PRegistry s, T s x a ))


type T s x a
    = PTask (Task.Task x (Proc s x a))
    | POk a
    | PErr x



--| PInitiate (Int -> Cmd (Proc s x a))
--| PExecute Int (Cmd (Proc s x a))
--| PSubscribe Int (Int -> Msg) (Int -> Sub (Proc s x a))
--| PUnsubscribe Int Int (Proc s x a)


type Msg
    = Initiate (Int -> Cmd Msg)
    | Execute Int (Cmd Msg)
    | Subscribe Int (Int -> Msg) (Int -> Sub Msg)
    | Unsubscribe Int Int Msg
    | Continue


type Procedure e a
    = Procedure (Int -> (Result e a -> Msg) -> Cmd Msg)


type Model
    = Model Registry


type alias Registry =
    { nextId : Int
    , channels : Dict Int (Sub Msg)
    }



-- Imperative programs


{-| Imperative Elm programs.
-}
type alias Program flags model err res =
    Platform.Program flags (PRegistry model) (Proc model err res)


{-| Builds an imperative program from flags, an initial model and an imperative program structure.
-}
program : flags -> (flags -> model) -> Proc model err res -> Program flags model err res
program flags initFn io =
    Platform.worker
        { init = \_ -> eval io (initFn flags |> initp)
        , update = eval
        , subscriptions = \_ -> Sub.none
        }


initp s =
    { nextId = 0
    , channels = Dict.empty
    , state = s
    }


eval : Proc s x a -> PRegistry s -> ( PRegistry s, Cmd (Proc s x a) )
eval (State io) reg =
    case io reg of
        ( innerS, PTask t ) ->
            ( innerS
            , Task.attempt
                (\r ->
                    case r of
                        Ok x ->
                            x

                        Err e ->
                            err e
                )
                t
            )

        ( innerS, POk x ) ->
            ( innerS
            , Cmd.none
            )

        ( innerS, PErr e ) ->
            ( innerS
            , Cmd.none
            )



-- Constructors


{-| Wraps a value as an `Proc`, bringing a pure value into the imperative structure.
-}
pure : a -> Proc s x a
pure val =
    (\s -> ( s, POk val ))
        |> State


{-| Builds an error as an `Proc`, allowing errors in imperative programs. This is
like the `throw` operation.
-}
err : x -> Proc s x a
err e =
    (\s -> ( s, PErr e ))
        |> State


{-| Turns an Elm task into an `Proc`, allowing some IO operation to be run, and that
may produce errors.
-}
task : Task.Task x a -> Proc s x a
task t =
    (\s -> ( s, t |> Task.map pure |> PTask ))
        |> State


{-| Convert `Result` into an `Proc`.
-}
result : Result x a -> Proc s x a
result res =
    case res of
        Ok x ->
            pure x

        Err e ->
            err e


{-| Given the current state of an imperative program, produce a new state and current
value for the program.
-}
advance : (s -> ( s, a )) -> Proc s x a
advance fn =
    (\reg ->
        let
            ( s, a ) =
                fn reg.state
        in
        ( { reg | state = s }, POk a )
    )
        |> State



-- Combinators for building imperative programs


{-| Given an `Proc` allows a new `Proc` to be created based on its current value.

This allows imperative operations to be chained together, when the subsequent operation
depends on the results of the one before.

-}
andThen : (a -> Proc s x b) -> Proc s x a -> Proc s x b
andThen mf (State io) =
    (\s ->
        case io s of
            ( innerS, PTask t ) ->
                ( innerS
                , Task.andThen (\inner -> Task.succeed (andThen mf inner)) t
                    |> PTask
                )

            ( innerS, POk x ) ->
                let
                    (State stateFn) =
                        mf x
                in
                stateFn innerS

            ( innerS, PErr e ) ->
                ( innerS
                , PErr e
                )
    )
        |> State


{-| Apply the function that is inside `Proc` to a value that is inside `Proc`. Return the result
inside `Proc`. If one of the `Proc`s in on the error track, the resulting `Proc` will also be on the
error track.
-}
andMap : Proc s x a -> Proc s x (a -> b) -> Proc s x b
andMap ma mf =
    andThen (\f -> map f ma) mf


{-| Given an `Proc` allows a new `Proc` to be created based on it being on the error track and
the value of the current error.

This allows imperative operations that produce errors to be chained together, with the option to
recover back onto the success track.

-}
onError : (x -> Proc s y a) -> Proc s x a -> Proc s y a
onError ef (State io) =
    (\s ->
        case io s of
            ( innerS, PTask t ) ->
                ( innerS
                , Task.onError
                    (\e -> ef e |> Task.succeed)
                    (t |> Task.map (onError ef))
                    |> PTask
                )

            ( innerS, POk x ) ->
                ( innerS, POk x )

            ( innerS, PErr e ) ->
                let
                    (State stateFn) =
                        ef e
                in
                stateFn innerS
    )
        |> State


{-| Applies a function the current value of an `Proc`, provided it is not on the error track.
-}
map : (a -> b) -> Proc s x a -> Proc s x b
map mf (State io) =
    (\s ->
        case io s of
            ( innerS, PTask t ) ->
                ( innerS
                , Task.andThen (\inner -> Task.succeed (map mf inner)) t |> PTask
                )

            ( innerS, POk x ) ->
                ( innerS
                , mf x |> POk
                )

            ( innerS, PErr e ) ->
                ( innerS
                , PErr e
                )
    )
        |> State



-- More maps


{-| Applies a function to current values of two `Proc`s if both `Proc`s are not on the error track.
-}
map2 :
    (a -> b -> c)
    -> Proc s x a
    -> Proc s x b
    -> Proc s x c
map2 f p1 p2 =
    pure f
        |> andMap p1
        |> andMap p2


{-| Applies a function to many values of many `Proc`s if all `Proc`s are not on the error track.
-}
map3 :
    (a -> b -> c -> d)
    -> Proc s x a
    -> Proc s x b
    -> Proc s x c
    -> Proc s x d
map3 f p1 p2 p3 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3


{-| Applies a function to many values of many `Proc`s if all `Proc`s are not on the error track.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Proc s x a
    -> Proc s x b
    -> Proc s x c
    -> Proc s x d
    -> Proc s x e
map4 f p1 p2 p3 p4 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4


{-| Applies a function to many values of many `Proc`s if all `Proc`s are not on the error track.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Proc s x a
    -> Proc s x b
    -> Proc s x c
    -> Proc s x d
    -> Proc s x e
    -> Proc s x f
map5 f p1 p2 p3 p4 p5 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5


{-| Applies a function to many values of many `Proc`s if all `Proc`s are not on the error track.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Proc s x a
    -> Proc s x b
    -> Proc s x c
    -> Proc s x d
    -> Proc s x e
    -> Proc s x f
    -> Proc s x g
map6 f p1 p2 p3 p4 p5 p6 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5
        |> andMap p6



-- State management


{-| An `Proc` that gets the current state as the current value.
-}
get : Proc s x s
get =
    State (\reg -> ( reg, POk reg.state ))


{-| An `Proc` that takes the given current state.
-}
put : s -> Proc s x ()
put s =
    State (\reg -> ( { reg | state = s }, POk () ))


{-| Applies a function to the current state to produce an `Proc` with a new current state.
-}
modify : (s -> s) -> Proc s x ()
modify fn =
    State (\reg -> ( { reg | state = fn reg.state }, POk () ))



-- Sequencing lists of imperatives


{-| Given a list of `Proc`s, evaluates them all and produces a list of their current
values, provided that none of them switch to the error track.

In the case where one or more of them produce errors, only the first error encountered
in the list will become the error value of the result.

-}
sequence : List (Proc s x a) -> Proc s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios



-- Procedure


fetch : ((a -> Msg) -> Cmd Msg) -> Procedure e a
fetch generator =
    (\_ tagger ->
        (Ok >> tagger) |> generator
    )
        |> Procedure


fetchResult : ((Result e a -> Msg) -> Cmd Msg) -> Procedure e a
fetchResult generator =
    (\_ tagger ->
        generator tagger
    )
        |> Procedure


do : Cmd Msg -> Procedure Never ()
do command =
    (\procId resultTagger ->
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
                )
    )
        |> Procedure


endWith : Cmd Msg -> Procedure Never Never
endWith command =
    (\procId _ ->
        Task.succeed ()
            |> Task.perform
                (\_ ->
                    Execute procId command
                )
    )
        |> Procedure


provide : a -> Procedure e a
provide =
    Task.succeed >> fromTask


fromTask : Task e a -> Procedure e a
fromTask t =
    (\_ resultTagger ->
        Task.attempt resultTagger t
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



--andThen : (a -> Procedure e b) -> Procedure e a -> Procedure e b
--andThen generator procedure =
--    (\aResult ->
--        case aResult of
--            Ok aData ->
--                generator aData
--
--            Err eData ->
--                break eData
--    )
--        |> next procedure
--collect : List (Procedure e a) -> Procedure e (List a)
--collect procedures =
--    case procedures of
--        [] ->
--            emptyProcedure
--
--        procedure :: remainingProcedures ->
--            List.foldl (addToList >> andThen) (addToList procedure []) remainingProcedures


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
    (\_ _ -> Cmd.none) |> Procedure



--map : (a -> b) -> Procedure e a -> Procedure e b
--map mapper =
--    andThen (mapper >> provide)
--
--
--map2 : (a -> b -> c) -> Procedure e a -> Procedure e b -> Procedure e c
--map2 mapper procedureA procedureB =
--    procedureA
--        |> andThen
--            (\aData ->
--                procedureB
--                    |> map (mapper aData)
--            )
--
--
--map3 : (a -> b -> c -> d) -> Procedure e a -> Procedure e b -> Procedure e c -> Procedure e d
--map3 mapper procedureA procedureB procedureC =
--    procedureA
--        |> andThen
--            (\aData ->
--                map2 (mapper aData) procedureB procedureC
--            )
--
--


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
    (\procId tagger ->
        (\aResult ->
            let
                (Procedure nextProcedure) =
                    resultMapper aResult
            in
            nextProcedure procId tagger
                |> Execute procId
        )
            |> procedure procId
    )
        |> Procedure


try : (Result e a -> Msg) -> Procedure e a -> Cmd Msg
try tagger (Procedure procedure) =
    Task.succeed (\procId -> procedure procId tagger)
        |> Task.perform Initiate


run : (a -> Msg) -> Procedure Never a -> Cmd Msg
run tagger =
    try
        (\res ->
            case res of
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
    (\procId resultTagger ->
        let
            requestCommandMsg channelId =
                channel.request (channelKey channelId)
                    |> Execute procId

            subGenerator channelId =
                (\aData ->
                    if channel.shouldAccept (channelKey channelId) aData then
                        generateMsg channelId aData

                    else
                        Continue
                )
                    |> channel.subscription

            generateMsg channelId aData =
                if shouldUnsubscribe aData then
                    Ok aData
                        |> resultTagger
                        |> Unsubscribe procId channelId

                else
                    Ok aData
                        |> resultTagger
        in
        Task.succeed subGenerator
            |> Task.perform (Subscribe procId requestCommandMsg)
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
