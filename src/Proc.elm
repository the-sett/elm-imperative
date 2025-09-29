module Proc exposing
    ( Proc
    , Program, program
    , Model, Protocol, init, subscriptions, update, run
    , pure, err, result, task, do, advance
    , andThen, andMap, onError, map
    , map2, map3, map4, map5, map6
    , get, put, modify
    , sequence
    , Channel, ChannelRequest
    , join, open, connect, filter
    , accept, acceptOne, acceptUntil
    )

{-| Proc provides a structure that combines 3 things; `Result`, `Procedure` and the
state monad. This helps you do stateful programming with IO and error handling.

@docs Proc


# Imperative programs

@docs Program, program


# TEA structure for hooking this into larger programs

@docs Model, Protocol, init, subscriptions, update, run


# Constructors

@docs pure, err, result, task, do, advance


# Combinators for building imperative programs

@docs andThen, andMap, onError, map


# More maps

@docs map2, map3, map4, map5, map6


# State management

@docs get, put, modify


# Sequencing lists of imperatives

@docs sequence


# Work with subscriptions or command together

@docs Channel, ChannelRequest
@docs join, open, connect, filter
@docs accept, acceptOne, acceptUntil

-}

import Dict exposing (Dict)
import Task exposing (Task)



-- The imperative structure


{-| Internal state that is needed to evaluate a Proc.
-}
type Model s x a
    = Registry
        { nextId : Int
        , channels : Dict Int (Sub (Proc s x a))
        , state : s
        }


{-| Proc combines `Result`, `Task` and the state monad together.
-}
type Proc s x a
    = Proc (s -> ( s, T s x a ))


type T s x a
    = PTask (Task.Task x (Proc s x a))
    | POk a
    | PErr x
    | PSubscribe (Int -> Proc s x a) (Int -> Sub (Proc s x a))
    | PUnsubscribe Int (Proc s x a)
    | PExecute (Cmd (Proc s x a))



-- Imperative programs


{-| Imperative Elm programs.
-}
type alias Program flags s x a =
    Platform.Program flags (Model s x a) (Proc s x a)


{-| Builds an imperative program from flags, an initial model and an imperative program structure.
-}
program : flags -> (flags -> s) -> Proc s x a -> Program flags s x a
program flags initFn io =
    let
        protocol =
            { toMsg = identity
            , onUpdate = identity
            , onReturn = \_ _ -> identity
            }
    in
    Platform.worker
        { init = \_ -> update protocol io (initFn flags |> init)
        , update = update protocol
        , subscriptions = subscriptions protocol
        }



-- TEA structure for hooking this into larger programs


{-| Protocol describes the outcomes possible when running a single step of the update loop for processing
a Proc and for lifting them into a parent update loop.

A Proc will typically do a normal 'onUpdate' many times and terminate with a single `onReturn`.

-}
type alias Protocol s x a submodel msg model =
    { toMsg : Proc s x a -> msg
    , onUpdate : ( submodel, Cmd (Proc s x a) ) -> ( model, Cmd msg )
    , onReturn : s -> Result x a -> ( submodel, Cmd (Proc s x a) ) -> ( model, Cmd msg )
    }


{-| Create the Model from some starting state.
-}
init : s -> Model s x a
init s =
    { nextId = 0
    , channels = Dict.empty
    , state = s
    }
        |> Registry


{-| Provides the subscriptions needed to evaluate against the Model.
-}
subscriptions : Protocol s x a (Model s x a) msg model -> Model s x a -> Sub msg
subscriptions protocol (Registry reg) =
    Dict.values reg.channels
        |> Sub.batch
        |> Sub.map protocol.toMsg


{-| Evalulates a Proc against a Model, producing a new model and some Cmds until the Proc is fully evaluated
and terminates.
-}
update : Protocol s x a (Model s x a) msg model -> Proc s x a -> Model s x a -> ( model, Cmd msg )
update protocol (Proc io) (Registry reg) =
    let
        addChannel subGenerator innerReg =
            { innerReg
                | nextId = innerReg.nextId + 1
                , channels = Dict.insert innerReg.nextId (subGenerator innerReg.nextId) innerReg.channels
            }

        deleteChannel channelId innerReg =
            { innerReg | channels = Dict.remove channelId innerReg.channels }
    in
    case io reg.state of
        ( nextS, PTask t ) ->
            ( { reg | state = nextS } |> Registry
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
                |> protocol.onUpdate

        ( nextS, POk x ) ->
            ( { reg | state = nextS } |> Registry
            , Cmd.none
            )
                |> protocol.onUpdate

        ( nextS, PErr e ) ->
            ( { reg | state = nextS } |> Registry
            , Cmd.none
            )
                |> protocol.onUpdate

        ( nextS, PSubscribe messageGenerator subGenerator ) ->
            let
                nextReg =
                    addChannel subGenerator reg
            in
            ( { nextReg | state = nextS } |> Registry
            , messageGenerator reg.nextId
                -- reg.nextId is correct here. nextReg.nextId contains the bumped value for the next subscription.
                |> sendMessage
            )
                |> protocol.onUpdate

        ( nextS, PUnsubscribe channelId nextMessage ) ->
            let
                nextReg =
                    deleteChannel channelId reg
            in
            ( { nextReg | state = nextS } |> Registry
            , sendMessage nextMessage
            )
                |> protocol.onUpdate

        ( nextS, PExecute cmd ) ->
            ( { reg | state = nextS } |> Registry
            , cmd
            )
                |> protocol.onUpdate


sendMessage : msg -> Cmd msg
sendMessage msg =
    Task.succeed () |> Task.perform (always msg)



-- Not sure yet what to do with these:
--try : (Result e a -> Msg) -> Procedure e a -> Cmd Msg
--try tagger (Procedure procedure) =
--    Task.succeed (\procId -> procedure procId tagger)
--        |> Task.perform Initiate
--
--
--run : (a -> Msg) -> Procedure Never a -> Cmd Msg
--run tagger =
--    try
--        (\res ->
--            case res of
--                Ok data ->
--                    tagger data
--
--                Err e ->
--                    never e
--        )


run : (Proc s x a -> msg) -> Proc s x a -> Cmd msg
run toMsg proc =
    Task.succeed proc
        |> Task.perform toMsg



-- Constructors


{-| Wraps a value as an `Proc`, bringing a pure value into the imperative structure.
-}
pure : a -> Proc s x a
pure val =
    (\s -> ( s, POk val ))
        |> Proc


{-| Builds an error as an `Proc`, allowing errors in imperative programs. This is
like the `throw` operation.
-}
err : x -> Proc s x a
err e =
    (\s -> ( s, PErr e ))
        |> Proc


{-| Turns an Elm task into an `Proc`, allowing some IO operation to be run, and that
may produce errors.
-}
task : Task.Task x a -> Proc s x a
task t =
    (\s -> ( s, t |> Task.map pure |> PTask ))
        |> Proc


{-| Turns a Cmd into a `Proc`, allowing an IO operation to be run that never produces errors.
-}
do : Cmd a -> Proc s x a
do command =
    (\s ->
        ( s, Cmd.map pure command |> PExecute )
    )
        |> Proc


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
    (\s ->
        let
            ( nextS, a ) =
                fn s
        in
        ( nextS, POk a )
    )
        |> Proc



-- Combinators for building imperative programs


{-| Given an `Proc` allows a new `Proc` to be created based on its current value.

This allows imperative operations to be chained together, when the subsequent operation
depends on the results of the one before.

-}
andThen : (a -> Proc s x b) -> Proc s x a -> Proc s x b
andThen mf (Proc io) =
    (\s ->
        case io s of
            ( nextS, PTask t ) ->
                ( nextS
                , Task.andThen (\inner -> Task.succeed (andThen mf inner)) t
                    |> PTask
                )

            ( nextS, POk x ) ->
                let
                    (Proc stateFn) =
                        mf x
                in
                stateFn nextS

            ( nextS, PErr e ) ->
                ( nextS
                , PErr e
                )

            ( nextS, PSubscribe generator subGenerator ) ->
                let
                    mappedGen =
                        generator >> andThen mf

                    mappedSubGen =
                        subGenerator >> Sub.map (andThen mf)
                in
                ( nextS
                , PSubscribe mappedGen mappedSubGen
                )

            ( nextS, PUnsubscribe channelId nextProc ) ->
                ( nextS
                , andThen mf nextProc |> PUnsubscribe channelId
                )

            ( nextS, PExecute command ) ->
                ( nextS
                , Cmd.map (\p -> andThen mf p) command |> PExecute
                )
    )
        |> Proc


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
onError ef (Proc io) =
    (\s ->
        case io s of
            ( nextS, PTask t ) ->
                ( nextS
                , Task.onError
                    (\e -> ef e |> Task.succeed)
                    (t |> Task.map (onError ef))
                    |> PTask
                )

            ( nextS, POk x ) ->
                ( nextS, POk x )

            ( nextS, PErr e ) ->
                let
                    (Proc stateFn) =
                        ef e
                in
                stateFn nextS

            ( nextS, PSubscribe generator subGenerator ) ->
                let
                    mappedGen =
                        generator >> onError ef

                    mappedSubGen =
                        subGenerator >> Sub.map (onError ef)
                in
                ( nextS
                , PSubscribe mappedGen mappedSubGen
                )

            ( nextS, PUnsubscribe channelId nextProc ) ->
                ( nextS
                , onError ef nextProc |> PUnsubscribe channelId
                )

            ( nextS, PExecute command ) ->
                ( nextS
                , Cmd.map (\p -> onError ef p) command |> PExecute
                )
    )
        |> Proc


mapBoth : (a -> b) -> (x -> y) -> Proc s x a -> Proc s y b
mapBoth mf ef (Proc io) =
    (\s ->
        case io s of
            ( nextS, PTask t ) ->
                ( nextS
                , t
                    |> Task.andThen (\inner -> Task.succeed (mapBoth mf ef inner))
                    |> Task.onError (\inner -> Task.fail (ef inner))
                    |> PTask
                )

            ( nextS, POk x ) ->
                ( nextS
                , mf x |> POk
                )

            ( nextS, PErr e ) ->
                ( nextS
                , ef e |> PErr
                )

            ( nextS, PSubscribe generator subGenerator ) ->
                let
                    mappedGen =
                        generator >> mapBoth mf ef

                    mappedSubGen =
                        subGenerator >> Sub.map (mapBoth mf ef)
                in
                ( nextS
                , PSubscribe mappedGen mappedSubGen
                )

            ( nextS, PUnsubscribe channelId nextProc ) ->
                ( nextS
                , mapBoth mf ef nextProc |> PUnsubscribe channelId
                )

            ( nextS, PExecute command ) ->
                ( nextS
                , PExecute (Cmd.map (mapBoth mf ef) command)
                )
    )
        |> Proc


mapError : (x -> y) -> Proc s x a -> Proc s y a
mapError ef =
    mapBoth identity ef


{-| Applies a function the current value of an `Proc`, provided it is not on the error track.
-}
map : (a -> b) -> Proc s x a -> Proc s x b
map mf =
    mapBoth mf identity



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
    Proc (\s -> ( s, POk s ))


{-| An `Proc` that takes the given current state.
-}
put : s -> Proc s x ()
put s =
    Proc (\_ -> ( s, POk () ))


{-| Applies a function to the current state to produce an `Proc` with a new current state.
-}
modify : (s -> s) -> Proc s x ()
modify fn =
    Proc (\s -> ( fn s, POk () ))



-- Sequencing lists of imperatives


{-| Given a list of `Proc`s, evaluates them all and produces a list of their current
values, provided that none of them switch to the error track.

In the case where one or more of them produce errors, only the first error encountered
in the list will become the error value of the result.

-}
sequence : List (Proc s x a) -> Proc s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios



-- Channel


{-| A channel represents a source of events that can be polled off the channel as a Proc.
-}
type Channel s x a
    = Channel
        { request : String -> Cmd (Proc s x a)
        , subscription : (a -> Proc s x a) -> Sub (Proc s x a)
        , shouldAccept : String -> a -> Bool
        }


{-| A ChannelRequest represnets a request to a channel that may trigger an event from that channel.
Typically a channel request is created from a Cmd, and then connected to a Sub, in order to form a
request/response pair that acts as a task port.
-}
type ChannelRequest s x a
    = ChannelRequest (String -> Cmd (Proc s x a))


{-| Join a subscription as a Channel producing Procs
-}
join : ((a -> Proc s x a) -> Sub (Proc s x a)) -> Channel s x a
join generator =
    Channel
        { request = defaultRequest
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


{-| Open a channel request with a Cmd.
-}
open : (String -> Cmd (Proc s x a)) -> ChannelRequest s x a
open =
    ChannelRequest


{-| Connect a channel request to a subscription that responds to the channel request.
-}
connect : ((a -> Proc s x a) -> Sub (Proc s x a)) -> ChannelRequest s x a -> Channel s x a
connect generator (ChannelRequest requestGenerator) =
    Channel
        { request = requestGenerator
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


{-| Filters a channel.
-}
filter : (String -> a -> Bool) -> Channel s x a -> Channel s x a
filter predicate (Channel channel) =
    Channel
        { channel | shouldAccept = predicate }


{-| Accepts a single event from a channel before closing it.
-}
acceptOne : Channel s x a -> Proc s x a
acceptOne =
    always True |> acceptUntil


{-| Accepts a single event from a channel but leaves it open so it can produce more events.
-}
accept : Channel s x a -> Proc s x a
accept =
    always False |> acceptUntil


{-| Accepts a single event from a channel and may continue to leave the channel open to producing more events
based on a predicate on the last item produced by the channel.
-}
acceptUntil : (a -> Bool) -> Channel s x a -> Proc s x a
acceptUntil shouldUnsubscribe (Channel channel) =
    (\s ->
        let
            requestCommandMsg : Int -> Proc s x a
            requestCommandMsg channelId =
                (\innerS ->
                    ( innerS
                    , channel.request (channelKey channelId)
                        |> PExecute
                    )
                )
                    |> Proc

            subGenerator : Int -> Sub (Proc s x a)
            subGenerator channelId =
                (\aData ->
                    if channel.shouldAccept (channelKey channelId) aData then
                        (\innerS ->
                            ( innerS
                            , generateMsg channelId aData
                            )
                        )
                            |> Proc

                    else
                        (\innerS ->
                            ( innerS
                            , POk aData
                            )
                        )
                            |> Proc
                )
                    |> channel.subscription

            generateMsg : Int -> a -> T s x a
            generateMsg channelId aData =
                if shouldUnsubscribe aData then
                    PUnsubscribe
                        (channelId |> Debug.log "PUnsubscribe:channelId")
                        (pure aData)

                else
                    POk aData
        in
        ( s
        , PSubscribe requestCommandMsg subGenerator
        )
    )
        |> Proc


channelKey : Int -> String
channelKey channelId =
    "Channel-" ++ String.fromInt channelId


defaultRequest : String -> Cmd (Proc s x a)
defaultRequest _ =
    Cmd.none


defaultPredicate : String -> a -> Bool
defaultPredicate _ _ =
    True
