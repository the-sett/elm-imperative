module Main exposing (..)

import Proc
import Task


type alias Example =
    { messages : List String }


example : Proc.Proc Example String ()
example =
    Proc.task (Task.succeed "success1")
        |> Proc.andThen push
        |> Proc.andThen (\_ -> Proc.err "error")
        |> Proc.andThen push
        |> Proc.onError recover
        |> Proc.andThen (\_ -> Proc.pure "success2")
        |> Proc.andThen push
        |> Proc.andThen (\_ -> Proc.task (Task.succeed "task"))
        |> Proc.andThen push
        |> Proc.andThen (\_ -> Proc.task (Task.fail "failed task"))
        |> Proc.andThen push
        |> Proc.andThen (\_ -> Proc.pure "skipped pure")
        |> Proc.andThen push
        |> Proc.andThen (\_ -> Proc.task (Task.succeed "skipped task"))
        |> Proc.andThen push
        |> Proc.onError recover
        |> Proc.andThen (\_ -> Proc.get)
        |> Proc.map (\s -> Debug.log "state" s)
        |> Proc.andThen (\_ -> Proc.modify (\state -> { state | messages = List.reverse state.messages }))


push : String -> Proc.Proc Example String ()
push msg =
    Proc.modify (\state -> { state | messages = msg :: state.messages })


recover : String -> Proc.Proc Example String ()
recover msg =
    Proc.pure ("recovered " ++ msg) |> Proc.andThen push


main =
    Proc.program
        ()
        (always { messages = [ "initial" ] })
        example
