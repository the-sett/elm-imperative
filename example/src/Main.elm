port module Main exposing (..)

import Proc
import Task
import Time


port request : String -> Cmd msg


port response : (String -> msg) -> Sub msg


type alias Example =
    { messages : List String }


roundTrip : String -> Proc.Proc s x String
roundTrip val =
    Proc.open (\_ -> request val)
        |> Proc.connect response
        |> Proc.acceptOne


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
        |> Proc.andThen (\_ -> roundTrip "task port")
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


example2 : Proc.Proc Example String ()
example2 =
    Proc.join (Time.every 500)
        |> Proc.accept
        |> Proc.map (Time.posixToMillis >> String.fromInt >> Debug.log "tick")
        |> Proc.andThen push
        |> Proc.andThen (\_ -> Proc.pure "success")
        |> Proc.andThen push
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
        example2
