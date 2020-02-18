module Viewer exposing (Viewer, cred, decoder, store, user)

import Api exposing (Cred)
import Http
import Json.Decode as Decode exposing (Decoder)
import User exposing (User)


type Viewer
    = Viewer User Cred



-- INFO


user : Viewer -> User
user (Viewer val _) =
    val


cred : Viewer -> Cred
cred (Viewer _ val) =
    val



-- SERIALIZATION


decoder : Decoder (User -> Cred -> Viewer)
decoder =
    Decode.succeed Viewer


store : Viewer -> Cmd msg
store (Viewer infoVal credVal) =
    Api.storeCredWith credVal infoVal
