module Proc exposing
    ( Proc
    , Program, program
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


# Work with subscriptions

@docs Channel, ChannelRequest
@docs join, open, connect, filter
@docs accept, acceptOne, acceptUntil

-}

import Dict exposing (Dict)
import Task exposing (Task)



-- The imperative structure


type alias PRegistry s x a =
    { nextId : Int
    , channels : Dict Int (Sub (Proc s x a))
    , state : s
    }


{-| Proc combines `Result`, `Task` and the state monad together.
-}
type Proc s x a
    = Proc (Int -> s -> ( s, T s x a ))


type T s x a
    = PTask (Task.Task x (Proc s x a))
    | POk a
    | PErr x
    | PInitiate (Int -> Cmd (Proc s x a))
    | PSubscribe Int (Int -> Proc s x a) (Int -> Sub (Proc s x a))
    | PUnsubscribe Int Int (Proc s x a)
    | PExecute Int (Cmd (Proc s x a))



-- Imperative programs


{-| Imperative Elm programs.
-}
type alias Program flags model err res =
    Platform.Program flags (PRegistry model err res) (Proc model err res)


{-| Builds an imperative program from flags, an initial model and an imperative program structure.
-}
program : flags -> (flags -> model) -> Proc model err res -> Program flags model err res
program flags initFn io =
    Platform.worker
        { init = \_ -> eval io (initFn flags |> initReg)
        , update = eval
        , subscriptions = subscriptions
        }


initReg s =
    { nextId = 0
    , channels = Dict.empty
    , state = s
    }


subscriptions : PRegistry s x a -> Sub (Proc s x a)
subscriptions registry =
    Dict.values registry.channels
        |> Sub.batch


eval : Proc s x a -> PRegistry s x a -> ( PRegistry s x a, Cmd (Proc s x a) )
eval (Proc io) reg =
    case io reg.nextId reg.state of
        ( nextS, PTask t ) ->
            ( { reg | state = nextS }
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

        ( nextS, POk x ) ->
            ( { reg | state = nextS }
            , Cmd.none
            )

        ( nextS, PErr e ) ->
            ( { reg | state = nextS }
            , Cmd.none
            )

        ( nextS, PInitiate generator ) ->
            ( { reg
                | nextId = reg.nextId + 1
                , state = nextS
              }
            , generator reg.nextId
            )

        ( nextS, PSubscribe _ messageGenerator subGenerator ) ->
            let
                nextReg =
                    addChannel subGenerator reg
            in
            ( { nextReg | state = nextS }
            , messageGenerator reg.nextId
                |> sendMessage
            )

        ( nextS, PUnsubscribe _ channelId nextMessage ) ->
            let
                nextReg =
                    deleteChannel channelId reg
            in
            ( { nextReg | state = nextS }
            , sendMessage nextMessage
            )

        ( nextS, PExecute _ cmd ) ->
            ( { reg | state = nextS }
            , cmd
            )


addChannel : (Int -> Sub (Proc s x a)) -> PRegistry s x a -> PRegistry s x a
addChannel subGenerator reg =
    { reg
        | nextId = reg.nextId + 1
        , channels = Dict.insert reg.nextId (subGenerator reg.nextId) reg.channels
    }


deleteChannel : Int -> PRegistry s x a -> PRegistry s x a
deleteChannel channelId reg =
    { reg | channels = Dict.remove channelId reg.channels }


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
-- Constructors


{-| Wraps a value as an `Proc`, bringing a pure value into the imperative structure.
-}
pure : a -> Proc s x a
pure val =
    (\_ s -> ( s, POk val ))
        |> Proc


{-| Builds an error as an `Proc`, allowing errors in imperative programs. This is
like the `throw` operation.
-}
err : x -> Proc s x a
err e =
    (\_ s -> ( s, PErr e ))
        |> Proc


{-| Turns an Elm task into an `Proc`, allowing some IO operation to be run, and that
may produce errors.
-}
task : Task.Task x a -> Proc s x a
task t =
    (\_ s -> ( s, t |> Task.map pure |> PTask ))
        |> Proc


{-| Turns a Cmd into a `Proc`, allowing an IO operation to be run that never produces errors.
-}
do : Cmd a -> Proc s Never a
do command =
    (\pid s ->
        ( s, Cmd.map pure command |> PExecute pid )
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
    (\pid s ->
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
    (\pid s ->
        case io pid s of
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
                stateFn pid nextS

            ( nextS, PErr e ) ->
                ( nextS
                , PErr e
                )

            ( nextS, PInitiate generator ) ->
                ( nextS
                , (generator >> Cmd.map (andThen mf)) |> PInitiate
                )

            ( nextS, PSubscribe procId generator subGenerator ) ->
                let
                    mappedGen =
                        generator >> andThen mf

                    mappedSubGen =
                        subGenerator >> Sub.map (andThen mf)
                in
                ( nextS
                , PSubscribe procId mappedGen mappedSubGen
                )

            ( nextS, PUnsubscribe procId channelId nextProc ) ->
                ( nextS
                , andThen mf nextProc |> PUnsubscribe procId channelId
                )

            ( nextS, PExecute procId command ) ->
                ( nextS
                , Cmd.map (\p -> andThen mf p) command |> PExecute procId
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
    (\pid s ->
        case io pid s of
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
                stateFn pid nextS

            ( nextS, PInitiate generator ) ->
                ( nextS
                , generator >> Cmd.map (\p -> onError ef p) |> PInitiate
                )

            ( nextS, PSubscribe procId generator subGenerator ) ->
                let
                    mappedGen =
                        generator >> onError ef

                    mappedSubGen =
                        subGenerator >> Sub.map (onError ef)
                in
                ( nextS
                , PSubscribe procId mappedGen mappedSubGen
                )

            ( nextS, PUnsubscribe procId channelId nextProc ) ->
                ( nextS
                , onError ef nextProc |> PUnsubscribe procId channelId
                )

            ( nextS, PExecute procId command ) ->
                ( nextS
                , Cmd.map (\p -> onError ef p) command |> PExecute procId
                )
    )
        |> Proc


mapBoth : (a -> b) -> (x -> y) -> Proc s x a -> Proc s y b
mapBoth mf ef (Proc io) =
    (\pid s ->
        case io pid s of
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

            ( nextS, PInitiate generator ) ->
                ( nextS
                , (generator >> Cmd.map (mapBoth mf ef)) |> PInitiate
                )

            ( nextS, PSubscribe procId generator subGenerator ) ->
                let
                    mappedGen =
                        generator >> mapBoth mf ef

                    mappedSubGen =
                        subGenerator >> Sub.map (mapBoth mf ef)
                in
                ( nextS
                , PSubscribe procId mappedGen mappedSubGen
                )

            ( nextS, PUnsubscribe procId channelId nextProc ) ->
                ( nextS
                , mapBoth mf ef nextProc |> PUnsubscribe procId channelId
                )

            ( nextS, PExecute procId command ) ->
                ( nextS
                , PExecute procId (Cmd.map (mapBoth mf ef) command)
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
    Proc (\_ s -> ( s, POk s ))


{-| An `Proc` that takes the given current state.
-}
put : s -> Proc s x ()
put s =
    Proc (\_ _ -> ( s, POk () ))


{-| Applies a function to the current state to produce an `Proc` with a new current state.
-}
modify : (s -> s) -> Proc s x ()
modify fn =
    Proc (\_ s -> ( fn s, POk () ))



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


type Channel s x a
    = Channel
        { request : String -> Cmd (Proc s x a)
        , subscription : (a -> Proc s x a) -> Sub (Proc s x a)
        , shouldAccept : String -> a -> Bool
        }


type ChannelRequest s x a
    = ChannelRequest (String -> Cmd (Proc s x a))


join : ((a -> Proc s x a) -> Sub (Proc s x a)) -> Channel s x a
join generator =
    Channel
        { request = defaultRequest
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


open : (String -> Cmd (Proc s x a)) -> ChannelRequest s x a
open =
    ChannelRequest


connect : ((a -> Proc s x a) -> Sub (Proc s x a)) -> ChannelRequest s x a -> Channel s x a
connect generator (ChannelRequest requestGenerator) =
    Channel
        { request = requestGenerator
        , subscription = generator
        , shouldAccept = defaultPredicate
        }


filter : (String -> a -> Bool) -> Channel s x a -> Channel s x a
filter predicate (Channel channel) =
    Channel
        { channel | shouldAccept = predicate }


acceptOne : Channel s x a -> Proc s x a
acceptOne =
    always True |> acceptUntil


accept : Channel s x a -> Proc s x a
accept =
    always False |> acceptUntil


acceptUntil : (a -> Bool) -> Channel s x a -> Proc s x a
acceptUntil shouldUnsubscribe (Channel channel) =
    (\pid s ->
        let
            requestCommandMsg : Int -> Proc s x a
            requestCommandMsg channelId =
                (\_ innerS ->
                    ( innerS
                    , channel.request (channelKey channelId)
                        |> PExecute pid
                    )
                )
                    |> Proc

            subGenerator : Int -> Sub (Proc s x a)
            subGenerator channelId =
                (\aData ->
                    if channel.shouldAccept (channelKey channelId) aData then
                        (\_ innerS ->
                            ( innerS
                            , generateMsg channelId aData
                            )
                        )
                            |> Proc

                    else
                        (\_ innerS ->
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
                    PUnsubscribe pid channelId (pure aData)

                else
                    POk aData
        in
        ( s
        , PSubscribe pid requestCommandMsg subGenerator
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
