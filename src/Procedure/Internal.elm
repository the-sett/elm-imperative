module Procedure.Internal exposing
    ( Msg(..)
    , Procedure(..)
    )


type Msg
    = Initiate (Int -> Cmd Msg)
    | Execute Int (Cmd Msg)
    | Subscribe Int (Int -> Msg) (Int -> Sub Msg)
    | Unsubscribe Int Int Msg
    | Continue


type Procedure e a
    = Procedure (Int -> (Msg -> Msg) -> (Result e a -> Msg) -> Cmd Msg)
