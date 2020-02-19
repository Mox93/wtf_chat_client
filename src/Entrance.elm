module Entrance exposing (Model, Msg, update, view)

import Api exposing (Cred)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Encode as Encode exposing (Value)
import Layout
import Util
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { email : String
    , password : String
    , showPassword : Bool
    , rememberMe : Bool
    }



-- UPDATE


type Msg
    = NoOp
    | Enter
    | ChangeEmail String
    | ChangePassword String
    | ToggleShowPassword Bool
    | ToggleRememberMe Bool
    | GotResponse (Result Http.Error (Maybe Viewer))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Enter ->
            ( model, Api.enter GotResponse Viewer.decoder <| encode model )

        ChangeEmail email ->
            ( { model | email = email }, Cmd.none )

        ChangePassword password ->
            ( { model | password = password }, Cmd.none )

        ToggleShowPassword show ->
            ( { model | showPassword = show }, Cmd.none )

        ToggleRememberMe remember ->
            ( { model | rememberMe = remember }, Cmd.none )

        GotResponse response ->
            case response of
                Ok (Just viewer) ->
                    ( model, Viewer.store viewer )

                _ ->
                    ( model, Cmd.none )



-- SERIALIZATION


encode : Model -> Value
encode model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "password", Encode.string model.password )
        , ( "remember_me", Encode.bool model.rememberMe )
        ]



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Entrance"
    , content = viewForm model
    }


viewForm : Model -> Element Msg
viewForm model =
    el [ centerY, centerX ] <|
        popOutEffect <|
            column
                [ padding 32
                , spacing 16
                , width (px 420)
                , htmlAttribute (Util.onEnterHandler Enter NoOp)
                ]
                [ Layout.viewHeader "WTF Chat"
                , Layout.viewEmailField model.email ChangeEmail
                , viewPasswordField model.password model.showPassword
                , viewEnterBtn
                ]


popOutEffect : Element msg -> Element msg
popOutEffect body =
    el
        [ Border.rounded 16
        , Border.shadow
            { offset = ( 8, 8 )
            , size = 8
            , blur = 8
            , color = rgba 0 0 0 0.1
            }
        ]
    <|
        el
            [ Border.rounded 16
            , Border.shadow
                { offset = ( -8, -8 )
                , size = 8
                , blur = 8
                , color = rgba 1 1 1 0.6
                }
            ]
            body


viewPasswordField : String -> Bool -> Element Msg
viewPasswordField password show =
    Input.currentPassword
        [ height (px 48)
        , Border.width 0
        , Border.rounded 24
        , Background.color (rgb 1 1 1)
        , inFront <| viewShowPassword show
        ]
        { onChange = ChangePassword
        , text = password
        , placeholder =
            Just
                (Input.placeholder
                    []
                    (text "password")
                )
        , label = Input.labelHidden "password"
        , show = show
        }


viewShowPassword : Bool -> Element Msg
viewShowPassword show =
    el
        [ centerY
        , alignRight
        , moveLeft 20
        ]
    <|
        Input.checkbox
            []
            { onChange = ToggleShowPassword
            , icon =
                \_ ->
                    image [ centerX, centerY ] <|
                        if show then
                            { src = "/assets/visibility-24px.svg"
                            , description = "vOn"
                            }

                        else
                            { src = "/assets/visibility_off-24px.svg"
                            , description = "vOff"
                            }
            , checked = show
            , label = Input.labelHidden "show password"
            }


viewEnterBtn : Element Msg
viewEnterBtn =
    el [ centerX ] <|
        Layout.viewPrimaryBtn
            { text = "Enter"
            , msg = Enter
            , size = ( 160, 48 )
            }



--Input.button
--    [ width (px 160)
--    , height (px 48)
--    , centerX
--    , Border.rounded 24
--    , Background.gradient { angle = -2.5, steps = [ rgb255 150 20 200, rgb255 75 25 225 ] }
--    ]
--    { onPress = Just Enter
--    , label =
--        el
--            [ centerX
--            , centerY
--            , Font.color (rgb 1 1 1)
--            , Font.bold
--            , Font.size 24
--            ]
--            (text "Enter")
--    }
