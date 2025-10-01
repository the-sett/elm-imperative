port module Main exposing (..)

import Proc
import Task
import Time


type alias Model =
    { procModel : Proc.Model
    , state : State
    }


type Msg
    = ProcMsg Proc.Msg


protocol : Model -> Proc.Protocol State String State Msg Model
protocol model =
    { toMsg = ProcMsg
    , onUpdate = Tuple.mapBoth (\pm -> { model | procModel = pm }) (Cmd.map ProcMsg)
    , onReturn =
        \s res ( pm, _ ) ->
            let
                _ =
                    Debug.log "Returned" { state = s, result = res }
            in
            ( { model | procModel = pm }, Cmd.none )
    , onThrow =
        \s res ( pm, _ ) ->
            let
                _ =
                    Debug.log "Returned" { state = s, result = res }
            in
            ( { model | procModel = pm }, Cmd.none )
    }


main : Platform.Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        exampleProc =
            example

        initState =
            { messages = [ "initial" ] }

        initModel =
            { procModel = Proc.init
            , state = { messages = [ "initial" ] }
            }

        ( nextState, cmd ) =
            Proc.run (protocol initModel) exampleProc initState
    in
    ( { procModel = Proc.init
      , state = nextState
      }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "Main.update" of
        ProcMsg proc ->
            let
                ( nextState, nextModel, cmd ) =
                    Proc.update (protocol model) proc model.procModel model.state
            in
            ( { nextModel | state = nextState }
            , cmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Proc.subscriptions (protocol model) model.procModel



-- Test ports


port request : String -> Cmd msg


port response : (String -> msg) -> Sub msg



-- Example Procs


type alias State =
    { messages : List String }


roundTrip : String -> Proc.Proc s x String
roundTrip val =
    Proc.open (\_ -> request val)
        |> Proc.connect response
        |> Proc.acceptOne


example : Proc.Proc State String State
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
        |> Proc.andThen (\_ -> Proc.get)


example2 : Int -> Proc.Proc State String Never
example2 n =
    if n == 0 then
        Proc.get
            |> Proc.map (\s -> Debug.log "state" s)
            |> Proc.return

    else
        (Proc.join (Time.every 500) |> Proc.acceptOne)
            |> Proc.map (Time.posixToMillis >> String.fromInt >> Debug.log "tick")
            |> Proc.andThen push
            |> Proc.andThen (\_ -> Proc.get)
            |> Proc.map (\s -> Debug.log "state" s)
            |> Proc.andThen (\_ -> example2 (n - 1))


push : String -> Proc.Proc State String ()
push msg =
    Proc.modify (\state -> { state | messages = msg :: state.messages })


recover : String -> Proc.Proc State String ()
recover msg =
    Proc.pure ("recovered " ++ msg) |> Proc.andThen push
