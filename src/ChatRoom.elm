module ChatRoom exposing (Model, Msg, init, subscriptions, update, view)

import Api exposing (Cred)
import Chat exposing (Chat, Chats)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Http
import Json.Decode as Decode exposing (Decoder)
import Message exposing (Message)
import User exposing (User)
import Util
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { viewer : Viewer
    , contacts : List User
    , chats : Chats
    }


init : Viewer -> ( Model, Cmd Msg )
init viewer =
    ( { viewer = viewer
      , contacts = []
      , chats = Chat.fromList []
      }
    , getChats <| Viewer.cred viewer
    )



-- UPDATE


type Msg
    = NoOp
    | SendMessage
    | Exit
    | SelectChat Chat
    | ChangText String
    | GotChats (Result Http.Error (List Chat))
    | GotMessageResponse (Result Http.Error ( String, Message, Message ))
    | GotNewMessage (Result Decode.Error ( String, Message ))
    | GotExitResponse (Result Http.Error Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SendMessage ->
            let
                ( chats, maybePair ) =
                    Chat.moveToBody model.chats (Viewer.user model.viewer)
            in
            ( { model | chats = chats }
            , case maybePair of
                Nothing ->
                    Cmd.none

                Just ( message, chat_id ) ->
                    sendMessage (Viewer.cred model.viewer) message chat_id
            )

        Exit ->
            ( model, Api.exit (Viewer.cred model.viewer) GotExitResponse )

        SelectChat chat ->
            ( { model | chats = Chat.select model.chats chat }, Cmd.none )

        ChangText text ->
            ( { model | chats = Chat.updateText model.chats text }, Cmd.none )

        GotChats response ->
            ( case response of
                Ok chats ->
                    { model | chats = Chat.fromList chats }

                Err _ ->
                    model
            , Cmd.none
            )

        GotMessageResponse response ->
            ( case response of
                Ok ( chat_id, oldMsg, newMsg ) ->
                    { model
                        | chats =
                            Chat.confirmMessage
                                { chats = model.chats, oldMsg = oldMsg, newMsg = newMsg, chat_id = chat_id }
                    }

                Err _ ->
                    model
            , Cmd.none
            )

        GotNewMessage response ->
            case response of
                Ok ( chat_id, message ) ->
                    ( { model
                        | chats = Chat.addMessage { chats = model.chats, chat_id = chat_id, msg = message }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotExitResponse response ->
            ( model
            , case response of
                Ok _ ->
                    Api.emptyCache

                Err _ ->
                    Cmd.none
            )



-- SERIALIZATION


subscriptions : Sub Msg
subscriptions =
    Api.newMessage GotNewMessage



-- HTTP


getChats : Cred -> Cmd Msg
getChats cred =
    Api.get
        { cred = cred
        , endpoint = "chats"
        , toMsg = GotChats
        , decoder = Chat.chatsDecoder
        }


sendMessage : Cred -> Message -> String -> Cmd Msg
sendMessage cred msg chat_id =
    Api.post
        { cred = cred
        , endpoint = "send-message"
        , value = Message.encode msg chat_id
        , toMsg = GotMessageResponse
        , decoder = Api.mainDecoder (Message.withOriginalDecoder msg)
        }



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Chat"
    , content =
        row
            [ width fill
            , height fill
            ]
            [ viewSideMenu model
            , viewChatBody model.chats (Viewer.user model.viewer)
            ]
    }


viewSideMenu : Model -> Element Msg
viewSideMenu model =
    column
        [ width (px 400)
        , height fill
        , alignLeft
        , Font.color (rgb 1 1 1)
        , Font.semiBold
        , Background.gradient { angle = -2, steps = [ rgb255 150 20 200, rgb255 75 25 225 ] }
        ]
        [ viewSideToolBar
        , viewChats model.chats
        ]


viewSideToolBar : Element Msg
viewSideToolBar =
    row
        [ height (px 72)
        , width fill
        , padding 12
        , spacing 12
        , Background.color (rgba 1 1 1 0.1)
        , Events.onMouseUp Exit
        ]
        [ image
            [ alignLeft
            , centerY
            , pointer
            ]
            { src = "/assets/exit_from_app-24px.svg"
            , description = "exit"
            }
        , el [ centerX, centerY ] <| text "Side Bar..."
        ]


viewChats : Chats -> Element Msg
viewChats chats =
    [ viewDivider ]
        ++ (Chat.toList chats
                |> List.map (\chat -> viewChatTab chat (Chat.selected chats == Just chat))
                |> List.intersperse viewDivider
           )
        ++ [ viewDivider ]
        |> column [ height fill, width fill ]


viewDivider : Element msg
viewDivider =
    el
        [ width fill
        , height (px 1)
        , Background.color (rgba 0 0 0 0.2)
        ]
        Element.none


viewChatTab : Chat -> Bool -> Element Msg
viewChatTab chat open =
    row
        [ width fill
        , height (px 72)
        , padding 12
        , spacing 12
        , pointer
        , Events.onMouseDown (SelectChat chat)
        , mouseOver
            [ Background.color <|
                if open then
                    rgba 0 0 0 0.2

                else
                    rgba 0 0 0 0.1
            ]
        , Background.color <|
            if open then
                rgba 0 0 0 0.2

            else
                rgba 0 0 0 0
        ]
        [ viewAvatar open
        , column
            []
            [ text <| Chat.title chat
            , Chat.emails chat
                |> String.join ", "
                |> text
                |> el [ Font.size 12 ]
            ]
        ]


viewAvatar : Bool -> Element msg
viewAvatar open =
    el
        [ width (px 48)
        , height (px 48)
        , Border.rounded 24
        , Background.color (rgb255 236 240 243)
        , clip
        ]
    <|
        el
            [ width (px 64)
            , height (px 64)
            , centerX
            , centerY
            , Border.rounded 36
            , moveDown 32
            , Background.color (rgb255 222 150 45)
            ]
        <|
            el
                [ width (px 32)
                , height (px 32)
                , centerX
                , centerY
                , Border.rounded 16
                , Border.color (rgb255 236 240 243)
                , Border.width 2
                , moveUp <|
                    if open then
                        48

                    else
                        42
                , Background.color (rgb255 222 150 45)
                ]
                Element.none


viewChatBody : Chats -> User -> Element Msg
viewChatBody chats sender =
    el
        [ width fill
        , height fill
        ]
    <|
        case Chat.selected chats of
            Nothing ->
                el [ centerX, centerY ] <| text "Chat Body..."

            Just chat ->
                column
                    [ width fill
                    , height fill
                    ]
                    [ viewChatToolBar chat
                    , viewMessages (Chat.messages chat) sender
                    , viewTextInput <| Chat.pendingMsg chat
                    ]


viewChatToolBar : Chat -> Element msg
viewChatToolBar chat =
    row
        [ alignTop
        , width fill
        , height (px 72)
        , Background.color (rgba 0 0 0 0.05)
        , padding 12
        , spacing 12
        ]
        [ viewAvatar True
        , text <| Chat.title chat
        ]


viewMessages : List Message -> User -> Element msg
viewMessages msgList user =
    column
        [ width fill
        , height fill
        , padding 24
        , spacing 12
        ]
    <|
        List.map (\msg -> Message.view msg user) msgList


viewTextInput : String -> Element Msg
viewTextInput msg =
    row
        [ alignBottom
        , width fill
        , height (minimum 72 shrink)
        , Background.color (rgba 0 0 0 0.05)
        , padding 12
        , spacing 12
        ]
        [ Input.multiline
            [ Border.rounded 24
            , Border.width 0
            , focused []
            , htmlAttribute (Util.onEnterHandler SendMessage NoOp)
            , Font.size 16
            , htmlAttribute (Attr.style "overflow-wrap" "break-word")
            ]
            { onChange = ChangText
            , text = msg
            , placeholder = Just <| Input.placeholder [] <| text "Type a message"
            , label = Input.labelHidden "msg"
            , spellcheck = True
            }
        ]
