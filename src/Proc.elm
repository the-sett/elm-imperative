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

import Procedure
import Procedure.Program as Program
import Task



-- The imperative structure


{-| Proc combines `Result`, `Task` and the state monad together.
-}
type Proc s x a
    = State (s -> ( s, T s x a ))
    | PMsg Program.Msg


type T s x a
    = PTask (Task.Task x (Proc s x a))
    | PProc (Procedure.Procedure x (Proc s x a))
    | POk a
    | PErr x



-- Imperative programs


{-| Imperative Elm programs.
-}
type alias Program flags model err res =
    Platform.Program flags model (Proc model err res)


{-| Builds an imperative program from flags, an initial model and an imperative program structure.
-}
program : flags -> (flags -> model) -> Proc model err res -> Program flags model err res
program flags initFn io =
    Platform.worker
        { init = \_ -> eval io (initFn flags)
        , update = eval
        , subscriptions = \_ -> Sub.none
        }


eval : Proc s x a -> s -> ( s, Cmd (Proc s x a) )
eval (State io) state =
    case io state of
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

        ( innerS, PProc p ) ->
            ( innerS
            , Procedure.try
                identity
                (\r ->
                    case r of
                        Ok x ->
                            x

                        Err e ->
                            err e
                )
                p
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
    (\s -> fn s |> Tuple.mapSecond POk)
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

            ( innerS, PProc p ) ->
                ( innerS
                , Procedure.andThen (\inner -> Procedure.provide (andThen mf inner)) p
                    |> PProc
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

            ( innerS, PProc p ) ->
                ( innerS
                , Procedure.catch
                    (\e -> ef e |> Procedure.provide)
                    (p |> Procedure.map (onError ef))
                    |> PProc
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

            ( innerS, PProc p ) ->
                ( innerS
                , Procedure.andThen (\inner -> Procedure.provide (map mf inner)) p |> PProc
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
    State (\s -> ( s, POk s ))


{-| An `Proc` that takes the given current state.
-}
put : s -> Proc s x ()
put s =
    State (\_ -> ( s, POk () ))


{-| Applies a function to the current state to produce an `Proc` with a new current state.
-}
modify : (s -> s) -> Proc s x ()
modify fn =
    State (\s -> ( fn s, POk () ))



-- Sequencing lists of imperatives


{-| Given a list of `Proc`s, evaluates them all and produces a list of their current
values, provided that none of them switch to the error track.

In the case where one or more of them produce errors, only the first error encountered
in the list will become the error value of the result.

-}
sequence : List (Proc s x a) -> Proc s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios
