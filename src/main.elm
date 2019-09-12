import OAuth
import OAuth.Implicit
import Url exposing (Url)
import Browser.Navigation as Navigation exposing (Key)

type alias Model =
    { redirectUri : Url
    , error : Maybe String
    , token : Maybe OAuth.Token
    , state : String
    }

type Msg = SignInRequested { clientId : String, authorizationEndpoint : String }

init : { randomBytes : String } -> Url -> Key -> ( Model, Cmd Msg )
init { randomBytes } origin _ =
    let
        model =
            { redirectUri = { origin | query = Nothing, fragment = Nothing }
            , error = Nothing
            , token = Nothing
            , state = randomBytes
            }
    in
    case OAuth.Implicit.parseToken origin of
        OAuth.Implicit.Empty ->
            ( model, Cmd.none )

        OAuth.Implicit.Success { token, state } ->
            if state /= Just model.state then
                ( { model | error = Just "'state' mismatch, request likely forged by an adversary!" }
                , Cmd.none
                )

            else
                ( { model | token = Just token }
                , getUserInfo config token
                )

        OAuth.Implicit.Error error ->
            ( { model | error = Just <| errorResponseToString error }
            , Cmd.none
            )
            
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignInRequested { clientId, authorizationEndpoint } ->
            let
                auth =
                    { clientId = clientId
                    , redirectUri = model.redirectUri
                    , scope = []
                    , state = Just model.state 
                    , url = authorizationEndpoint
                    }
            in
            ( model
            , auth |> OAuth.Implicit.makeAuthorizationUrl |> Url.toString |> Navigation.load
            )