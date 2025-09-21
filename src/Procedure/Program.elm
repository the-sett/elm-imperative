module Procedure.Program exposing (Model, Msg, init, subscriptions, update)

import Dict exposing (Dict)
import Procedure.Internal exposing (Msg(..))
import Task


type alias Msg =
    Procedure.Internal.Msg


type Model
    = Model Registry


type alias Registry =
    { nextId : Int
    , channels : Dict Int (Sub Msg)
    }


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
