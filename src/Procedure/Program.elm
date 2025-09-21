module Procedure.Program exposing (Msg, Model, init, update, subscriptions)

{-| Use these functions to configure your program to run procedures.

@docs Msg, Model, init, update, subscriptions

-}

import Dict exposing (Dict)
import Procedure.Internal exposing (ChannelId, Msg(..))
import Task


{-| Represents the internal `Msg` values used to track the state of a procedure.

The type variable refers to the `Msg` type used by your application.
You should provide a message type that wraps these values like so:

    type AppMsg
        = ProcMsg (Procedure.Program.Msg AppMsg)

-}
type alias Msg =
    Procedure.Internal.Msg


{-| Represents the internal state used to track running procedures.

You should store this in your application's model like so:

    type alias AppModel =
        { procModel : Procedure.Program.Model AppMsg
        }

-}
type Model
    = Model Registry


type alias Registry =
    { nextId : ChannelId
    , channels : Dict ChannelId (Sub Msg)
    }


{-| Generate the model used to track procedures.
-}
init : Model
init =
    { nextId = 0
    , channels = Dict.empty
    }
        |> Model


{-| Update the state of running procedures.

You should add this to your application's update function like so:

    update : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
    update appMsg appModel =
        case appMsg of
            ProcedureTagger procMsg ->
                Procedure.Program.update procMsg appModel.procModel
                    |> Tuple.mapFirst (\updated -> { appModel | procModel = updated })

-}
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


addChannel : (ChannelId -> Sub Msg) -> Registry -> Registry
addChannel subGenerator registry =
    { registry
        | nextId = registry.nextId + 1
        , channels = Dict.insert registry.nextId (subGenerator registry.nextId) registry.channels
    }


deleteChannel : ChannelId -> Registry -> Registry
deleteChannel channelId procModel =
    { procModel | channels = Dict.remove channelId procModel.channels }


sendMessage : Msg -> Cmd Msg
sendMessage msg =
    Task.succeed ()
        |> Task.perform (always msg)


{-| Get any subscriptions necessary for running procedures.

Add this to your application's subscriptions function like so:

    appSubscriptions : AppModel -> Sub AppMsg
    appSubscriptions appModel =
        Procedure.Program.subscriptions appModel.procModel

Note: You only need to use this function if you are using procedures
with channels, i.e. if you have subscriptions in your procedures.

-}
subscriptions : Model -> Sub Msg
subscriptions (Model registry) =
    Dict.values registry.channels
        |> Sub.batch
