module Util exposing (..)

import Html
import Html.Events exposing (keyCode, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)



-- EVENTS


onEnterHandler : msg -> msg -> Html.Attribute msg
onEnterHandler send noOp =
    Decode.map4
        (\key shift ctrl alt ->
            { code = key, shift = shift, ctrl = ctrl, alt = alt }
        )
        keyCode
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        |> Decode.andThen
            (\keys ->
                if keys.code == 13 then
                    if keys.shift then
                        Decode.succeed ( noOp, False )

                    else if keys.ctrl || keys.alt then
                        Decode.succeed ( noOp, True )

                    else
                        Decode.succeed ( send, True )

                else
                    Decode.fail "Not Enter Key"
            )
        |> preventDefaultOn "keydown"



-- EXPORT


pass : model -> ( model, Cmd msg )
pass model =
    ( model, Cmd.none )
