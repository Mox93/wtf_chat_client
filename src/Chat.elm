module Chat exposing
    ( Chat
    , Chats
    , addChat
    , addMessage
    , chatsDecoder
    , confirmMessage
    , decoder
    , emails
    , fromList
    , id
    , messages
    , moveToBody
    , name
    , pendingMsg
    , select
    , selected
    , title
    , toList
    , updateText
    , view
    )

import Api exposing (Cred)
import Element exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Message exposing (Message)
import User exposing (User)



-- TYPE


type alias PersonalChatRecord =
    { id : String
    , recipient : User
    , messages : List Message
    , pendingMsg : String
    }


type alias GroupChatRecord =
    { id : String
    , name : Maybe String
    , members : List User
    , messages : List Message
    , pendingMsg : String
    }


type Chat
    = PersonalChat PersonalChatRecord
    | GroupChat GroupChatRecord


type Chats
    = Idle (List Chat)
    | Active (List Chat) Chat (List Chat)



--INFO


id : Chat -> String
id chat =
    case chat of
        PersonalChat body ->
            body.id

        GroupChat body ->
            body.id


pendingMsg : Chat -> String
pendingMsg chat =
    case chat of
        PersonalChat body ->
            body.pendingMsg

        GroupChat body ->
            body.pendingMsg


name : Chat -> Maybe String
name chat =
    case chat of
        PersonalChat body ->
            body.recipient.userName

        GroupChat body ->
            body.name


title : Chat -> String
title chat =
    case chat of
        PersonalChat body ->
            Maybe.withDefault "No Name" body.recipient.userName

        GroupChat body ->
            Maybe.withDefault "Untitled" body.name


emails : Chat -> List String
emails chat =
    case chat of
        PersonalChat body ->
            [ body.recipient.email ]

        GroupChat body ->
            List.map .email body.members


messages : Chat -> List Message
messages chat =
    case chat of
        PersonalChat body ->
            body.messages

        GroupChat body ->
            body.messages


selected : Chats -> Maybe Chat
selected chats =
    case chats of
        Active _ val _ ->
            Just val

        Idle _ ->
            Nothing



-- CONVENTION


fromList : List Chat -> Chats
fromList chatList =
    Idle chatList


toList : Chats -> List Chat
toList chats =
    case chats of
        Idle val ->
            val

        Active val1 val2 val3 ->
            val1 ++ [ val2 ] ++ val3



-- EXTERNAL


select : Chats -> Chat -> Chats
select chats chat =
    case chats of
        Idle chatList ->
            case split chatList chat of
                Nothing ->
                    Idle chatList

                Just ( val1, val2, val3 ) ->
                    Active val1 val2 val3

        Active val1 val2 val3 ->
            case ( split val1 chat, split val3 chat ) of
                ( Just ( valA, valB, valC ), Nothing ) ->
                    Active valA valB (valC ++ [ val2 ] ++ val3)

                ( Nothing, Just ( valA, valB, valC ) ) ->
                    Active (val1 ++ [ val2 ] ++ valA) valB valC

                ( _, _ ) ->
                    Active val1 val2 val3


updateText : Chats -> String -> Chats
updateText chats text =
    case chats of
        Idle _ ->
            chats

        Active val1 val2 val3 ->
            Active val1 (changeText val2 text) val3


moveToBody : Chats -> User -> ( Chats, Maybe ( Message, String ) )
moveToBody chats sender =
    case chats of
        Idle _ ->
            ( chats, Nothing )

        Active val1 val2 val3 ->
            let
                trimmedMsg =
                    String.trim (pendingMsg val2)
            in
            if trimmedMsg == "" then
                ( chats, Nothing )

            else
                let
                    ( chat, msg ) =
                        relocateMsg (changeText val2 trimmedMsg) sender
                in
                ( Active val1 chat val3, Just ( msg, id chat ) )


confirmMessage : { r | chats : Chats, chat_id : String, oldMsg : Message, newMsg : Message } -> Chats
confirmMessage config =
    let
        updateChatMsg chat =
            if config.chat_id == id chat then
                case chat of
                    PersonalChat pChat ->
                        PersonalChat <| Message.replace pChat config.oldMsg config.newMsg

                    GroupChat gChat ->
                        GroupChat <| Message.replace gChat config.oldMsg config.newMsg

            else
                chat
    in
    case config.chats of
        Idle chatList ->
            Idle (List.map updateChatMsg chatList)

        Active val1 val2 val3 ->
            Active (List.map updateChatMsg val1) (updateChatMsg val2) (List.map updateChatMsg val3)


addMessage : { r | chats : Chats, chat_id : String, msg : Message } -> Chats
addMessage config =
    let
        updateChatMsg chat =
            if config.chat_id == id chat then
                case chat of
                    PersonalChat pChat ->
                        PersonalChat { pChat | messages = pChat.messages ++ [ config.msg ] }

                    GroupChat gChat ->
                        GroupChat { gChat | messages = gChat.messages ++ [ config.msg ] }

            else
                chat
    in
    case config.chats of
        Idle chatList ->
            Idle (List.map updateChatMsg chatList)

        Active val1 val2 val3 ->
            Active (List.map updateChatMsg val1) (updateChatMsg val2) (List.map updateChatMsg val3)


addChat : Chats -> Chat -> Chats
addChat chats newChat =
    if List.member newChat (toList chats) then
        select chats newChat

    else
        Active [] newChat <| toList chats



--INTERNAL


split : List Chat -> Chat -> Maybe ( List Chat, Chat, List Chat )
split chatList chat =
    if List.member chat chatList then
        let
            index =
                List.indexedMap Tuple.pair chatList
                    |> List.filter (\pair -> Tuple.second pair == chat)
                    |> List.map Tuple.first
                    |> List.head
        in
        case index of
            Nothing ->
                Nothing

            Just idx ->
                let
                    val1 =
                        List.take idx chatList

                    val2 =
                        List.drop (idx + 1) chatList
                in
                Just ( val1, chat, val2 )

    else
        Nothing


changeText : Chat -> String -> Chat
changeText chat text =
    case chat of
        PersonalChat body ->
            PersonalChat { body | pendingMsg = text }

        GroupChat body ->
            GroupChat { body | pendingMsg = text }


relocateMsg : Chat -> User -> ( Chat, Message )
relocateMsg chat sender =
    let
        msg =
            Message.fromString (pendingMsg chat) sender

        forward body =
            { body
                | messages = body.messages ++ [ msg ]
                , pendingMsg = ""
            }
    in
    case chat of
        PersonalChat body ->
            ( PersonalChat (forward body), msg )

        GroupChat body ->
            ( GroupChat (forward body), msg )



-- SERIALIZATION


decoder : Decoder Chat
decoder =
    Decode.oneOf
        [ Decode.map GroupChat <|
            Decode.map5 GroupChatRecord
                (Decode.field "_id" Decode.string)
                (Decode.field "name" <| Decode.nullable Decode.string)
                (Decode.field "members" <| Decode.list User.decoder)
                (Decode.succeed [])
                (Decode.succeed "")
        , Decode.map PersonalChat <|
            Decode.map4 PersonalChatRecord
                (Decode.field "_id" Decode.string)
                (Decode.field "recipient" <| User.decoder)
                (Decode.succeed [])
                (Decode.succeed "")
        ]


chatsDecoder : Decoder (List Chat)
chatsDecoder =
    Api.mainDecoder <| Decode.field "chats" <| Decode.list decoder



-- VIEW


view : List Message -> User -> Element msg
view msgList user =
    column
        [ width fill
        , height fill
        , paddingXY 72 24
        , spacing 12
        ]
    <|
        List.map (\msg -> Message.view msg user) msgList
