port module Api exposing
    ( Cred
    , delete
    , document
    , emptyCache
    , enter
    , exit
    , get
    , mainDecoder
    , newMessage
    , post
    , storeCredWith
    , viewerChanges
    )

import Base64
import Browser
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Message exposing (Message)
import User exposing (User)



-- CREDENTIALS


type Cred
    = Cred RefreshToken AccessToken


type AccessToken
    = AccessToken (Maybe String) Int


type RefreshToken
    = RefreshToken String Int


type alias JWT =
    { exp : Int
    , identity : User
    }



-- SERIALIZATION


mainDecoder : Decoder a -> Decoder a
mainDecoder subDecoder =
    Decode.field "data" subDecoder


credDecoder : Decoder Cred
credDecoder =
    Decode.map2 Cred
        (Decode.map toRefreshToken (Decode.field "refresh" Decode.string))
        (Decode.map toAccessToken (Decode.maybe (Decode.field "access" Decode.string)))


toAccessToken : Maybe String -> AccessToken
toAccessToken maybeToken =
    case decodeJWT <| Maybe.withDefault "" maybeToken of
        Ok json ->
            AccessToken maybeToken (json.exp * 1000)

        Err _ ->
            AccessToken maybeToken 0


toRefreshToken : String -> RefreshToken
toRefreshToken token =
    case decodeJWT token of
        Ok json ->
            RefreshToken token json.exp

        Err _ ->
            RefreshToken token 0


jwtDecoder : Decoder JWT
jwtDecoder =
    Decode.map2 JWT
        (Decode.field "exp" Decode.int)
        (Decode.field "identity" <| User.decoder)


decodeJWT : String -> Result Decode.Error JWT
decodeJWT jwt =
    let
        parts =
            String.split "." jwt
    in
    case parts of
        [ _, payload, _ ] ->
            Base64.decode payload
                |> Result.withDefault ""
                |> Decode.decodeString jwtDecoder

        val ->
            Err <| Decode.Failure "Incorrect JWT Format" (Encode.list Encode.string val)


credToViewerDecoder : Decoder (User -> Cred -> viewer) -> Cred -> Decoder (Maybe viewer)
credToViewerDecoder toViewer cred =
    let
        (Cred _ (AccessToken maybeToken _)) =
            cred

        jsonResult =
            decodeJWT <| Maybe.withDefault "" maybeToken
    in
    case jsonResult of
        Ok json ->
            Decode.map (\createViewer -> Just <| createViewer json.identity cred) toViewer

        Err _ ->
            Decode.succeed Nothing



-- HTTP


url_root =
    --"http://localhost:5000/api/"
    --"http://192.168.0.108:5000/api/"
    "http://192.168.0.113:5000/api/"


enter :
    { r
        | toMsg : Result Http.Error (Maybe viewer) -> msg
        , toViewer : Decoder (User -> Cred -> viewer)
        , value : Value
    }
    -> Cmd msg
enter request =
    Http.post
        { url = url_root ++ "enter"
        , body = Http.jsonBody request.value
        , expect =
            Http.expectJson request.toMsg <|
                Decode.andThen (credToViewerDecoder request.toViewer) (mainDecoder credDecoder)
        }


post :
    { r
        | cred : Cred
        , endpoint : String
        , value : Value
        , toMsg : Result Http.Error value -> msg
        , decoder : Decoder value
    }
    -> Cmd msg
post request =
    let
        (Cred _ (AccessToken maybeToken _)) =
            request.cred
    in
    Http.request
        { method = "POST"
        , headers =
            case maybeToken of
                Nothing ->
                    []

                Just token ->
                    [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = url_root ++ request.endpoint
        , body = Http.jsonBody request.value
        , expect = Http.expectJson request.toMsg request.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


get :
    { r
        | cred : Cred
        , endpoint : String
        , toMsg : Result Http.Error value -> msg
        , decoder : Decoder value
    }
    -> Cmd msg
get request =
    let
        (Cred _ (AccessToken maybeToken _)) =
            request.cred
    in
    Http.request
        { method = "GET"
        , headers =
            case maybeToken of
                Nothing ->
                    []

                Just token ->
                    [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = url_root ++ request.endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson request.toMsg request.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete :
    { r
        | cred : Cred
        , endpoint : String
        , value : Value
        , toMsg : Result Http.Error value -> msg
        , decoder : Decoder value
    }
    -> Cmd msg
delete request =
    let
        (Cred _ (AccessToken maybeToken _)) =
            request.cred
    in
    Http.request
        { method = "DELETE"
        , headers =
            case maybeToken of
                Nothing ->
                    []

                Just token ->
                    [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = url_root ++ request.endpoint
        , body = Http.jsonBody request.value
        , expect = Http.expectJson request.toMsg request.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


exit : Cred -> (Result Http.Error Bool -> msg) -> Cmd msg
exit cred toMsg =
    let
        (Cred (RefreshToken token _) _) =
            cred
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = url_root ++ "exit"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (mainDecoder (Decode.field "ok" Decode.bool))
        , timeout = Nothing
        , tracker = Nothing
        }



-- PERSISTENCE


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (User -> Cred -> viewer) -> Sub msg
viewerChanges toMsg viewerDecoder =
    onStoreChange
        (\value -> toMsg (decodeFromChange viewerDecoder value))


decodeFromChange : Decoder (User -> Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Cred -> User -> Cmd msg
storeCredWith (Cred (RefreshToken rToken _) (AccessToken aToken _)) user =
    let
        json =
            Encode.object
                [ ( "viewer"
                  , Encode.object
                        [ ( "user", User.encode user )
                        , ( "access"
                          , case aToken of
                                Just token ->
                                    Encode.string token

                                Nothing ->
                                    Encode.null
                          )
                        , ( "refresh", Encode.string rToken )
                        ]
                  )
                ]
    in
    storeCache (Just json)


emptyCache : Cmd msg
emptyCache =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg



-- SOCKET_IO


port receiveMessage : (Value -> msg) -> Sub msg


newMessage : (Result Decode.Error ( String, Message ) -> msg) -> Sub msg
newMessage toMsg =
    receiveMessage (\value -> toMsg (Decode.decodeValue (mainDecoder Message.withIdDecoder) value))



-- DOCUMENT


flagDecoder : Decoder (User -> Cred -> viewer) -> Decoder { url : String, maybeViewer : Maybe viewer }
flagDecoder viewerDecoder =
    Decode.map2
        (\url storage ->
            let
                maybeViewer =
                    Decode.decodeString (storageDecoder viewerDecoder) storage
                        |> Result.toMaybe
            in
            { url = url
            , maybeViewer = maybeViewer
            }
        )
        (Decode.field "url" Decode.string)
        (Decode.field "viewer" Decode.string)


document :
    Decoder (User -> Cred -> viewer)
    ->
        { init : String -> Maybe viewer -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
document viewerDecoder config =
    let
        init_ flags =
            let
                { url, maybeViewer } =
                    Decode.decodeValue (flagDecoder viewerDecoder) flags
                        |> Result.withDefault { url = "", maybeViewer = Nothing }
            in
            config.init url maybeViewer
    in
    Browser.document
        { init = init_
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (User -> Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "viewer" (decoderFromCred viewerDecoder)


decoderFromCred : Decoder (User -> Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map3 (\fromCred user cred -> fromCred user cred)
        decoder
        (Decode.field "user" User.decoder)
        credDecoder
