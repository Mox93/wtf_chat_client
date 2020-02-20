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
import Json.Encode as Encode exposing (Value)
import Layout
import Message exposing (Message)
import User exposing (User)
import Util exposing (pass)
import Viewer exposing (Viewer)



-- MODEL


type DialogBox
    = NewContact String


type alias Model =
    { viewer : Viewer
    , contacts : List User
    , chats : Chats
    , dialogBox : Maybe DialogBox
    }


init : Viewer -> ( Model, Cmd Msg )
init viewer =
    ( { viewer = viewer
      , contacts = []
      , chats = Chat.fromList []
      , dialogBox = Nothing
      }
    , getChats <| Viewer.cred viewer
    )



-- UPDATE


type Msg
    = NoOp
    | Exit
    | SendMessage
    | AddChat
    | SelectChat Chat
    | ChangText String
    | ChangeEmail String
    | ToggleDialogBox (Maybe DialogBox)
    | GotChats (Result Http.Error (List Chat))
    | GotMessageResponse (Result Http.Error ( String, Message, Message ))
    | GotNewMessage (Result Decode.Error ( String, Message ))
    | GotExitResponse (Result Http.Error Bool)
    | GotNewChat (Result Http.Error Chat)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            pass model

        Exit ->
            ( model, Api.exit (Viewer.cred model.viewer) GotExitResponse )

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

        AddChat ->
            case model.dialogBox of
                Nothing ->
                    pass model

                Just (NewContact email) ->
                    ( model, newChat (Viewer.cred model.viewer) email )

        SelectChat chat ->
            pass { model | chats = Chat.select model.chats chat }

        ChangText text ->
            pass { model | chats = Chat.updateText model.chats text }

        ToggleDialogBox maybeDialogBox ->
            pass { model | dialogBox = maybeDialogBox }

        ChangeEmail email ->
            case model.dialogBox of
                Nothing ->
                    pass model

                Just (NewContact _) ->
                    pass { model | dialogBox = Just (NewContact email) }

        GotChats response ->
            pass <|
                case response of
                    Ok chats ->
                        { model | chats = Chat.fromList chats }

                    Err _ ->
                        model

        GotMessageResponse response ->
            pass <|
                case response of
                    Ok ( chat_id, oldMsg, newMsg ) ->
                        { model
                            | chats =
                                Chat.confirmMessage
                                    { chats = model.chats, oldMsg = oldMsg, newMsg = newMsg, chat_id = chat_id }
                        }

                    Err _ ->
                        model

        GotNewMessage response ->
            case response of
                Ok ( chat_id, message ) ->
                    pass
                        { model
                            | chats = Chat.addMessage { chats = model.chats, chat_id = chat_id, msg = message }
                        }

                Err _ ->
                    pass model

        GotExitResponse response ->
            case response of
                Ok _ ->
                    ( model, Api.emptyCache )

                Err _ ->
                    pass model

        GotNewChat response ->
            let
                _ =
                    Debug.log "GotNewChat" response
            in
            case response of
                Ok chat ->
                    pass
                        { model
                            | chats = Chat.addChat model.chats chat
                            , dialogBox = Nothing
                        }

                Err _ ->
                    pass model



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


newChat : Cred -> String -> Cmd Msg
newChat cred email =
    Api.post
        { cred = cred
        , endpoint = "new-chat"
        , value = Encode.object [ ( "recipient", Encode.string email ) ]
        , toMsg = GotNewChat
        , decoder = Api.mainDecoder Chat.decoder
        }



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    let
        attr =
            case model.dialogBox of
                Nothing ->
                    []

                Just (NewContact email) ->
                    [ inFront <|
                        el [ width fill, height fill, Background.color (rgba 1 1 1 0.5) ] <|
                            viewAddContactBox email
                    ]
    in
    { title = "Chat"
    , content =
        row
            ([ width fill
             , height fill
             ]
                ++ attr
            )
            [ viewSideMenu model
            , viewChatBody model.chats (Viewer.user model.viewer)
            ]
    }


viewAddContactBox : String -> Element Msg
viewAddContactBox email =
    column
        [ width shrink
        , height shrink
        , centerX
        , centerY
        , padding 32
        , spacing 16
        , Background.color (rgb255 236 240 243)
        , Border.rounded 16
        , htmlAttribute (Util.onEnterHandler AddChat NoOp)
        , Border.shadow
            { offset = ( 2, 2 )
            , size = 2
            , blur = 2
            , color = rgba 0 0 0 0.1
            }
        ]
        [ el
            [ width fill
            , inFront <| viewCloseDialogBox
            ]
          <|
            Layout.viewHeader "Enter Email"
        , Layout.viewEmailField email ChangeEmail
        , viewAddBtn
        ]


viewCloseDialogBox : Element Msg
viewCloseDialogBox =
    image
        [ alignRight
        , alignTop
        , pointer
        , Events.onMouseUp (ToggleDialogBox Nothing)
        ]
        { src = "/assets/close-24px.svg"
        , description = "X"
        }


viewAddBtn : Element Msg
viewAddBtn =
    el [ centerX ] <|
        Layout.viewPrimaryBtn
            { text = "Add"
            , msg = AddChat
            , size = ( 100, 48 )
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
        ]
        [ viewExitBtn
        , el [ centerX, centerY ] <| text "WTF Chat"
        , viewAddChatBtn
        ]


viewExitBtn : Element Msg
viewExitBtn =
    image
        [ alignLeft
        , paddingXY 12 12
        , Border.rounded 24
        , centerY
        , pointer
        , mouseOver [ Background.color (rgba 0 0 0 0.1) ]
        , Events.onMouseUp Exit
        ]
        { src = "/assets/exit_from_app-24px.svg"
        , description = "exit"
        }


viewAddChatBtn : Element Msg
viewAddChatBtn =
    image
        [ alignRight
        , paddingXY 12 12
        , Border.rounded 24
        , centerY
        , pointer
        , mouseOver [ Background.color (rgba 0 0 0 0.1) ]
        , Events.onMouseUp (ToggleDialogBox <| Just <| NewContact "")
        ]
        { src = "/assets/add_comment-24px.svg"
        , description = "+"
        }


viewChats : Chats -> Element Msg
viewChats chats =
    [ viewDivider ]
        ++ (Chat.toList chats
                |> List.map (\chat -> viewChatCard chat (Chat.selected chats == Just chat))
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


viewChatCard : Chat -> Bool -> Element Msg
viewChatCard chat open =
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
                    , Chat.view (Chat.messages chat) sender
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
