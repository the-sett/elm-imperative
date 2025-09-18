module Main exposing (..)

import Imp
import Task


type alias Example =
    { messages : List String }


example : Imp.Imp Example String ()
example =
    Imp.task (Task.succeed "success1")
        |> Imp.andThen push
        |> Imp.andThen (\_ -> Imp.err "error")
        |> Imp.andThen push
        |> Imp.onError recover
        |> Imp.andThen (\_ -> Imp.pure "success2")
        |> Imp.andThen push
        |> Imp.andThen (\_ -> Imp.task (Task.succeed "task"))
        |> Imp.andThen push
        |> Imp.andThen (\_ -> Imp.task (Task.fail "failed task"))
        |> Imp.andThen push
        |> Imp.andThen (\_ -> Imp.pure "skipped pure")
        |> Imp.andThen push
        |> Imp.andThen (\_ -> Imp.task (Task.succeed "skipped task"))
        |> Imp.andThen push
        |> Imp.onError recover
        |> Imp.andThen (\_ -> Imp.get)
        |> Imp.map (\s -> Debug.log "state" s)
        |> Imp.andThen (\_ -> Imp.modify (\state -> { state | messages = List.reverse state.messages }))


push : String -> Imp.Imp Example String ()
push msg =
    Imp.modify (\state -> { state | messages = msg :: state.messages })


recover : String -> Imp.Imp Example String ()
recover msg =
    Imp.pure ("recovered " ++ msg) |> Imp.andThen push


main =
    Imp.program
        ()
        (always { messages = [ "initial" ] })
        example
