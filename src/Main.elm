module Main exposing (..)

import Api
import Browser
import ChatRoom
import Element exposing (..)
import Element.Background as Background
import Entrance
import Viewer exposing (Viewer)



-- MAIN


main =
    Api.document Viewer.decoder
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type State
    = Guest Entrance.Model
    | User ChatRoom.Model


type alias Model =
    { state : State
    , url : String
    }


init : String -> Maybe Viewer -> ( Model, Cmd Msg )
init url maybeViewer =
    let
        _ =
            Debug.log "maybeViewer" maybeViewer
    in
    case maybeViewer of
        Just viewer ->
            let
                ( model, cmd ) =
                    ChatRoom.init viewer
            in
            ( { url = url, state = User model }, Cmd.map GotChatRoomMsg cmd )

        Nothing ->
            ( { url = ""
              , state =
                    Guest
                        { email = "", password = "", rememberMe = False, showPassword = False }
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = GotEntranceMsg Entrance.Msg
    | GotChatRoomMsg ChatRoom.Msg
    | GotViewer (Maybe Viewer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEntranceMsg subMsg ->
            case model.state of
                Guest entrance ->
                    let
                        ( subModel, subCmd ) =
                            Entrance.update subMsg entrance
                    in
                    ( { model | state = Guest subModel }, Cmd.map GotEntranceMsg subCmd )

                _ ->
                    ( model, Cmd.none )

        GotChatRoomMsg subMsg ->
            case model.state of
                User chatRoom ->
                    let
                        ( subModel, subCmd ) =
                            ChatRoom.update subMsg chatRoom
                    in
                    ( { model | state = User subModel }
                    , Cmd.map GotChatRoomMsg subCmd
                    )

                _ ->
                    ( model, Cmd.none )

        GotViewer maybeViewer ->
            case ( maybeViewer, model.state ) of
                ( Nothing, Guest _ ) ->
                    ( model, Cmd.none )

                ( Just viewer, User chatRoom ) ->
                    ( { model | state = User { chatRoom | viewer = viewer } }, Cmd.none )

                ( _, _ ) ->
                    init model.url maybeViewer



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Api.viewerChanges (\maybeViewer -> GotViewer maybeViewer) Viewer.decoder
        , case model.state of
            Guest _ ->
                Sub.none

            User _ ->
                Sub.map GotChatRoomMsg ChatRoom.subscriptions
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage { title, content } toMsg =
            { title = "ChatRoom - " ++ title
            , body =
                [ layout
                    [ Background.color (rgb255 236 240 243)
                    ]
                    (Element.map toMsg content)
                ]
            }
    in
    case model.state of
        Guest entrance ->
            viewPage (Entrance.view entrance) GotEntranceMsg

        User chatRoom ->
            viewPage (ChatRoom.view chatRoom) GotChatRoomMsg
