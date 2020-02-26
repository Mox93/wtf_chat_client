module Message exposing
    ( Message
    , body
    , decoder
    , encode
    , fromString
    , markAsScene
    , replace
    , scene
    , sender
    , timeStamp
    , view
    , withIdDecoder
    , withOriginalDecoder
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
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
    , scene : Bool
    }


type alias PendingMsg =
    { sender : User
    , body : String
    , timeStamp : Time.Posix
    }


type Message
    = Incoming ConfirmedMsg
    | Outgoing PendingMsg



-- CONVERTER


fromString : String -> User -> Message
fromString msg user =
    Outgoing { sender = user, body = msg, timeStamp = Time.millisToPosix 0 }



-- INFO


body : Message -> String
body message =
    case message of
        Incoming msg ->
            msg.body

        Outgoing msg ->
            msg.body


sender : Message -> User
sender message =
    case message of
        Incoming msg ->
            msg.sender

        Outgoing msg ->
            msg.sender


timeStamp : Message -> Time.Posix
timeStamp message =
    case message of
        Incoming msg ->
            msg.timeStamp

        Outgoing msg ->
            msg.timeStamp


scene : Message -> Bool
scene message =
    case message of
        Incoming msg ->
            msg.scene

        Outgoing _ ->
            True



-- SERIALIZATION


encode : Message -> String -> Value
encode message chatId =
    case message of
        Incoming msg ->
            Encode.object
                [ ( "_id", Encode.string msg.id )
                , ( "chat_id", Encode.string chatId )
                , ( "body", Encode.string msg.body )
                , ( "time_stamp", Encode.int <| Time.posixToMillis msg.timeStamp )
                ]

        Outgoing msg ->
            Encode.object
                [ ( "body", Encode.string msg.body )
                , ( "chat_id", Encode.string chatId )
                , ( "time_stamp", Encode.int <| Time.posixToMillis msg.timeStamp )
                ]


decoder : Bool -> Decoder Message
decoder wasScene =
    Decode.map Incoming <|
        Decode.map5 ConfirmedMsg
            (Decode.field "_id" Decode.string)
            (Decode.field "sender" User.decoder)
            (Decode.field "body" Decode.string)
            (Decode.field "time_stamp" (Decode.map Time.millisToPosix Decode.int))
            (Decode.succeed wasScene)


withOriginalDecoder : Message -> Decoder ( String, Message, Message )
withOriginalDecoder oldMsg =
    Decode.map2 (\chatId newMsg -> ( chatId, oldMsg, newMsg ))
        (Decode.field "chat_id" Decode.string)
        (decoder True)


withIdDecoder : Decoder ( String, Message )
withIdDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "chat_id" Decode.string)
        (decoder False)



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


markAsScene : Message -> Message
markAsScene message =
    case message of
        Incoming msg ->
            Incoming { msg | scene = True }

        _ ->
            message



-- VIEW


view : Message -> User -> Element msg
view msg from =
    let
        senderStyle =
            if from == sender msg then
                [ alignRight
                , Background.color (rgb255 250 250 250)
                ]

            else
                [ alignLeft
                , Background.color (rgb255 209 220 231)
                ]
    in
    column
        ([ width (maximum 640 shrink)
         , height shrink
         , alignBottom
         , padding 12
         , spacing 8
         , Border.rounded 8
         ]
            ++ senderStyle
        )
    <|
        [ viewMsgBody (body msg)
        , viewStatus msg from
        ]


viewMsgBody : String -> Element msg
viewMsgBody msg =
    paragraph
        [ spacing 4
        , Font.size 16
        , htmlAttribute (Attr.style "word-break" "break-word")
        ]
        (String.split "\n" msg
            |> List.map text
            |> List.intersperse (html <| Html.br [] [])
        )


viewStatus : Message -> User -> Element msg
viewStatus msg from =
    let
        time =
            String.fromInt <| Time.posixToMillis <| timeStamp msg

        status =
            case msg of
                Outgoing _ ->
                    Background.color (rgb255 231 101 58)

                Incoming _ ->
                    if sender msg == from then
                        Background.color (rgb255 122 231 78)

                    else
                        Background.color (rgb255 110 150 231)
    in
    row
        [ Font.size 12
        , spacing 8
        , alignRight
        ]
        [ text time
        , el
            [ width (px 8)
            , height (px 8)
            , Border.rounded 4
            , status
            ]
            Element.none
        ]
