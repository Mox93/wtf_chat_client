module User exposing (User, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- USER


type alias User =
    { email : String
    , userName : Maybe String
    }



-- SERIALIZATION


decoder : Decoder User
decoder =
    Decode.map2 User
        (Decode.field "email" Decode.string)
        (Decode.field "user_name" <| Decode.nullable Decode.string)


encode : User -> Value
encode user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "user_name"
          , case user.userName of
                Nothing ->
                    Encode.null

                Just userName ->
                    Encode.string userName
          )
        ]
