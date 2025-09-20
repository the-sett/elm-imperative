module Procedure.Internal exposing
    ( ChannelId
    , Msg(..)
    , Procedure(..)
    , ProcedureId
    )


type alias ProcedureId =
    Int


type alias ChannelId =
    Int


type Msg
    = Initiate (ProcedureId -> Cmd Msg)
    | Execute ProcedureId (Cmd Msg)
    | Subscribe ProcedureId (ChannelId -> Msg) (ChannelId -> Sub Msg)
    | Unsubscribe ProcedureId ChannelId Msg
    | Continue


type Procedure e a
    = Procedure (ProcedureId -> (Msg -> Msg) -> (Result e a -> Msg) -> Cmd Msg)
