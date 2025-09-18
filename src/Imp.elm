module Imp exposing
    ( Imp
    , Program, program
    , pure, err, result, task, advance
    , andThen, andMap, onError, map
    , map2, map3, map4, map5, map6
    , get, put, modify
    , sequence
    )

{-| Imp provides a structure that combines 3 things; `Result`, `Task` and the
state monad. This helps you do stateful programming with IO and error handling.

@docs Imp


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

import Task



-- The imperative structure


{-| Imp combines `Result`, `Task` and the state monad together.
-}
type Imp s x a
    = State (s -> ( s, T s x a ))


type T s x a
    = PTask (Task.Task x (Imp s x a))
    | POk a
    | PErr x



-- Imperative programs


{-| Imperative Elm programs.
-}
type alias Program flags model err res =
    Platform.Program flags model (Imp model err res)


{-| Builds an imperative program from flags, an initial model and an imperative program structure.
-}
program : flags -> (flags -> model) -> Imp model err res -> Program flags model err res
program flags initFn io =
    Platform.worker
        { init = \_ -> eval io (initFn flags)
        , update = eval
        , subscriptions = \_ -> Sub.none
        }


eval : Imp s x a -> s -> ( s, Cmd (Imp s x a) )
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

        ( innerS, POk x ) ->
            ( innerS
            , Cmd.none
            )

        ( innerS, PErr e ) ->
            ( innerS
            , Cmd.none
            )



-- Constructors


{-| Wraps a value as an `Imp`, bringing a pure value into the imperative structure.
-}
pure : a -> Imp s x a
pure val =
    (\s -> ( s, POk val ))
        |> State


{-| Builds an error as an `Imp`, allowing errors in imperative programs. This is
like the `throw` operation.
-}
err : x -> Imp s x a
err e =
    (\s -> ( s, PErr e ))
        |> State


{-| Turns an Elm task into an `Imp`, allowing some IO operation to be run, and that
may produce errors.
-}
task : Task.Task x a -> Imp s x a
task t =
    (\s -> ( s, t |> Task.map pure |> PTask ))
        |> State


{-| Convert `Result` into an `Imp`.
-}
result : Result x a -> Imp s x a
result res =
    case res of
        Ok x ->
            pure x

        Err e ->
            err e


{-| Given the current state of an imperative program, produce a new state and current
value for the program.
-}
advance : (s -> ( s, a )) -> Imp s x a
advance fn =
    (\s -> fn s |> Tuple.mapSecond POk)
        |> State



-- Combinators for building imperative programs


{-| Given an `Imp` allows a new `Imp` to be created based on its current value.

This allows imperative operations to be chained together, when the subsequent operation
depends on the results of the one before.

-}
andThen : (a -> Imp s x b) -> Imp s x a -> Imp s x b
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


{-| Apply the function that is inside `Imp` to a value that is inside `Imp`. Return the result
inside `Imp`. If one of the `Imp`s in on the error track, the resulting `Imp` will also be on the
error track.
-}
andMap : Imp s x a -> Imp s x (a -> b) -> Imp s x b
andMap ma mf =
    andThen (\f -> map f ma) mf


{-| Given an `Imp` allows a new `Imp` to be created based on it being on the error track and
the value of the current error.

This allows imperative operations that produce errors to be chained together, with the option to
recover back onto the success track.

-}
onError : (x -> Imp s y a) -> Imp s x a -> Imp s y a
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


{-| Applies a function the current value of an `Imp`, provided it is not on the error track.
-}
map : (a -> b) -> Imp s x a -> Imp s x b
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


{-| Applies a function to current values of two `Imp`s if both `Imp`s are not on the error track.
-}
map2 :
    (a -> b -> c)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
map2 f p1 p2 =
    pure f
        |> andMap p1
        |> andMap p2


{-| Applies a function to many values of many `Imp`s if all `Imp`s are not on the error track.
-}
map3 :
    (a -> b -> c -> d)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
map3 f p1 p2 p3 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3


{-| Applies a function to many values of many `Imp`s if all `Imp`s are not on the error track.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
    -> Imp s x e
map4 f p1 p2 p3 p4 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4


{-| Applies a function to many values of many `Imp`s if all `Imp`s are not on the error track.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
    -> Imp s x e
    -> Imp s x f
map5 f p1 p2 p3 p4 p5 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5


{-| Applies a function to many values of many `Imp`s if all `Imp`s are not on the error track.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
    -> Imp s x e
    -> Imp s x f
    -> Imp s x g
map6 f p1 p2 p3 p4 p5 p6 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5
        |> andMap p6



-- State management


{-| An `Imp` that gets the current state as the current value.
-}
get : Imp s x s
get =
    State (\s -> ( s, POk s ))


{-| An `Imp` that takes the given current state.
-}
put : s -> Imp s x ()
put s =
    State (\_ -> ( s, POk () ))


{-| Applies a function to the current state to produce an `Imp` with a new current state.
-}
modify : (s -> s) -> Imp s x ()
modify fn =
    State (\s -> ( fn s, POk () ))



-- Sequencing lists of imperatives


{-| Given a list of `Imp`s, evaluates them all and produces a list of their current
values, provided that none of them switch to the error track.

In the case where one or more of them produce errors, only the first error encountered
in the list will become the error value of the result.

-}
sequence : List (Imp s x a) -> Imp s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios
