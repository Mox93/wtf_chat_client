module Message exposing
    ( Message
    , decoder
    , encode
    , fromString
    , replace
    , sender
    , view
    , withIdDecoder
    , withOriginalDecoder
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time
import User exposing (User)



-- TYPE


type alias ConfirmedMsg =
    { id : String
    , sender : User
    , body : String
    , timeStamp : Time.Posix
    }


type alias PendingMsg =
    { sender : User
    , body : String
    , timeStamp : Time.Posix
    }


type Message
    = Confirmed ConfirmedMsg
    | Pending PendingMsg



-- CONVERTER


fromString : String -> User -> Message
fromString msg user =
    Pending { sender = user, body = msg, timeStamp = Time.millisToPosix 0 }



-- INFO


body : Message -> String
body message =
    case message of
        Confirmed msg ->
            msg.body

        Pending msg ->
            msg.body


sender : Message -> User
sender message =
    case message of
        Confirmed msg ->
            msg.sender

        Pending msg ->
            msg.sender



-- SERIALIZATION


encode : Message -> String -> Value
encode message chat_id =
    case message of
        Confirmed msg ->
            Encode.object
                [ ( "_id", Encode.string msg.id )
                , ( "chat_id", Encode.string chat_id )
                , ( "body", Encode.string msg.body )
                , ( "time_stamp", Encode.int <| Time.posixToMillis msg.timeStamp )
                ]

        Pending msg ->
            Encode.object
                [ ( "body", Encode.string msg.body )
                , ( "chat_id", Encode.string chat_id )
                , ( "time_stamp", Encode.int <| Time.posixToMillis msg.timeStamp )
                ]


decoder : Decoder Message
decoder =
    Decode.map Confirmed <|
        Decode.map4 ConfirmedMsg
            (Decode.field "_id" Decode.string)
            (Decode.field "sender" User.decoder)
            (Decode.field "body" Decode.string)
            (Decode.field "time_stamp" (Decode.map Time.millisToPosix Decode.int))


withOriginalDecoder : Message -> Decoder ( String, Message, Message )
withOriginalDecoder msg =
    Decode.map2 (\chat_id m -> ( chat_id, msg, m ))
        (Decode.field "chat_id" Decode.string)
        decoder


withIdDecoder : Decoder ( String, Message )
withIdDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "chat_id" Decode.string)
        decoder



-- EXTERNAL


replace : { chat | messages : List Message } -> Message -> Message -> { chat | messages : List Message }
replace chat oldMsg newMsg =
    { chat
        | messages =
            List.map
                (\msg ->
                    if msg == oldMsg then
                        newMsg

                    else
                        msg
                )
                chat.messages
    }



-- VIEW


view : Message -> User -> Element msg
view msg from =
    el
        [ width (maximum 640 shrink)
        , height shrink
        , alignBottom
        , padding 12
        , Background.color (rgb255 209 220 231)
        , Border.rounded 8
        , Border.color (rgb255 231 101 58)
        , Font.size 16
        , if from == sender msg then
            alignRight

          else
            alignLeft
        , case msg of
            Pending _ ->
                Border.width 2

            Confirmed _ ->
                Border.width 0
        ]
    <|
        viewText (body msg)


viewText : String -> Element msg
viewText msg =
    column
        [ spacing 4 ]
        (String.split "\n" msg
            |> List.map
                (\m ->
                    paragraph
                        [ width fill
                        , spacing 4
                        , htmlAttribute (Attr.style "overflow-wrap" "break-word")
                        ]
                        [ text m ]
                )
        )
