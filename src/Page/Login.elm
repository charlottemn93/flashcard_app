module Page.Login exposing (Model, Msg, initialModel, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Ports exposing (attemptLogIn)



-- MODEL


type Model
    = LoggedOut


initialModel : Model
initialModel =
    LoggedOut



-- UPDATE


type Msg
    = AttemptLogIn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptLogIn ->
            ( model, attemptLogIn () )



-- VIEW


view : Html Msg
view =
    div []
        [ div [ class "text-center marginT40" ]
            [ text "Log in to the flashcard app" ]
        , div [ class "text-center" ]
            [ button
                [ attribute "data-test" "ButtonLogin"
                , class "loginBtn loginBtn--google"
                , attribute "data-onsuccess" "onSignIn"
                , onClick AttemptLogIn
                ]
                [ text "Log in with Google" ]
            ]
        ]
