module Testing exposing (..)

import Element exposing (..)
import Element.Border as Border
import Html.Attributes as Attr


main =
    layout [] <|
        row
            [ width fill
            , htmlAttribute <| Attr.style "height" "100vh"

            --, clipY
            --, htmlAttribute <| Attr.style "flex-shrink" "1"
            ]
            [ column [ width fill, height fill, clip, scrollbarY, Border.width 5 ] <|
                List.repeat 20 <|
                    el [ height (px 100), width fill ] <|
                        text "Testing Left"
            , column [ width fill, height fill, clipY, Border.width 5 ] <|
                List.repeat 5 <|
                    el [ height (px 100), width fill ] <|
                        text "Testing Right"
            ]
