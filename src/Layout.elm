module Layout exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input


viewHeader : String -> Element msg
viewHeader header =
    el
        [ centerX
        , Font.size 32
        , Font.bold
        ]
        (text header)


viewPrimaryBtn : { text : String, msg : msg, size : ( Int, Int ) } -> Element msg
viewPrimaryBtn config =
    let
        ( x, y ) =
            config.size

        size =
            if modBy y 2 == 0 then
                round (toFloat y / 2)

            else
                round (toFloat y / 2) + 1

        angle =
            atan (toFloat x / toFloat y)
    in
    el
        [ width (px x)
        , height (px y)
        , Border.rounded size
        , Background.gradient { angle = angle, steps = [ rgb255 150 20 200, rgb255 75 25 225 ] }
        , Events.onMouseUp config.msg
        , pointer
        ]
    <|
        el
            [ centerX
            , centerY
            , Font.color (rgb 1 1 1)
            , Font.bold
            , Font.size size
            ]
            (text config.text)


viewEmailField : String -> (String -> msg) -> Element msg
viewEmailField email msg =
    Input.email
        [ height (px 48)
        , width fill
        , Border.width 0
        , Border.rounded 24
        , Background.color (rgb 1 1 1)
        ]
        { onChange = msg
        , text = email
        , placeholder = Just (Input.placeholder [] (text "email"))
        , label = Input.labelHidden "email"
        }
