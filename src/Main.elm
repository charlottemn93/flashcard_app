module Main exposing (Msg(..), update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Credentials exposing (Credentials, credentialsDecoder)
import Element exposing (Element, el, fill, paddingEach, text, width)
import ElementLibrary.Helpers exposing (edges)
import Environment as Env exposing (Environment)
import Json.Decode as Decode
import Page.Login as LoginPage
import Page.ManageFlashcards as ManageFlashcardsPage
import Page.MyAccount as MyAccountPage
import Page.Revise as RevisePage
import Ports.Login exposing (logInSuccessful)
import Route exposing (Route(..), routeFromUrl)
import Toolbar exposing (toolbarElement)
import Url exposing (Url)



-- MODEL


type alias Flags =
    { idToken : Maybe String
    , accessToken : Maybe String
    , environment : String
    }


type alias Model =
    { cred : Maybe Credentials
    , state : State
    , navKey : Nav.Key
    , environment : Environment
    }


type State
    = ViewingManageFlashcards ManageFlashcardsPage.Model
    | ViewingLogin LoginPage.Model
    | ViewingRevise RevisePage.Model
    | ViewingMyAccount MyAccountPage.Model
    | Problem String



-- UPDATE


type Msg
    = ChangedUrl Url
    | ManageFlashcardsMsg ManageFlashcardsPage.Msg
    | ActivatedLink Browser.UrlRequest
    | LoginMsg LoginPage.Msg
    | MyAccountMsg MyAccountPage.Msg
    | ReviseMsg RevisePage.Msg
    | LogInSuccessful (Result Decode.Error Credentials)


pageNotFoundProblemState : State
pageNotFoundProblemState =
    Problem "Page not found =["


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.cred of
        Nothing ->
            case msg of
                LogInSuccessful (Ok cred) ->
                    let
                        ( state, command ) =
                            ManageFlashcardsPage.initialModel cred.idToken
                    in
                    ( { model
                        | cred = Just cred
                        , state = ViewingManageFlashcards state
                      }
                    , Cmd.map ManageFlashcardsMsg command
                    )

                LogInSuccessful (Err error) ->
                    ( { model | state = Problem <| Decode.errorToString error }, Cmd.none )

                LoginMsg loginMsg ->
                    case model.state of
                        ViewingLogin loginModel ->
                            let
                                ( updatedLoginModel, loginCommand ) =
                                    LoginPage.update loginMsg loginModel
                            in
                            ( { model | state = ViewingLogin updatedLoginModel }
                            , Cmd.map LoginMsg loginCommand
                            )

                        _ ->
                            ( { model | state = Problem "Problem: Received unexpected login msg when state was not in viewing login page." }
                            , Cmd.none
                            )

                _ ->
                    ( { model | state = Problem "Error: this can only be done once logged in." }
                    , Cmd.none
                    )

        Just { idToken } ->
            case msg of
                ActivatedLink urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model, Nav.pushUrl model.navKey (Url.toString url) )

                        Browser.External href ->
                            ( model, Nav.load href )

                ChangedUrl url ->
                    let
                        route =
                            routeFromUrl url
                    in
                    case route of
                        ManageFlashcards ->
                            let
                                ( state, command ) =
                                    ManageFlashcardsPage.initialModel idToken
                            in
                            ( { model
                                | state = ViewingManageFlashcards state
                              }
                            , Cmd.map ManageFlashcardsMsg command
                            )

                        Revise ->
                            ( { model | state = ViewingRevise <| RevisePage.initialModel }
                            , Cmd.none
                            )

                        MyAccount ->
                            ( { model | state = ViewingMyAccount <| MyAccountPage.initialModel }
                            , Cmd.none
                            )

                        PageNotFound ->
                            ( { model | state = pageNotFoundProblemState }
                            , Cmd.none
                            )

                ManageFlashcardsMsg manageFlashcardsMsg ->
                    case model.state of
                        ViewingManageFlashcards manageFlashcardsModel ->
                            let
                                ( updatedManageFlashCardsModel, manageFlashcardsCommand ) =
                                    ManageFlashcardsPage.update manageFlashcardsMsg manageFlashcardsModel
                            in
                            ( { model | state = ViewingManageFlashcards updatedManageFlashCardsModel }
                            , Cmd.map ManageFlashcardsMsg manageFlashcardsCommand
                            )

                        _ ->
                            ( { model | state = Problem "Problem: Received unexpected manage flashcards msg when state was not in viewing flashcards page." }
                            , Cmd.none
                            )

                LoginMsg loginMsg ->
                    case model.state of
                        ViewingLogin loginModel ->
                            let
                                ( updatedLoginModel, loginCommand ) =
                                    LoginPage.update loginMsg loginModel
                            in
                            ( { model | state = ViewingLogin updatedLoginModel }
                            , Cmd.map LoginMsg loginCommand
                            )

                        _ ->
                            ( { model | state = Problem "Problem: Received unexpected login msg when state was not in viewing login page." }
                            , Cmd.none
                            )

                MyAccountMsg myAccountMsg ->
                    case model.state of
                        ViewingMyAccount myAccountModel ->
                            let
                                ( updatedMyAccountModel, myAccountCommand ) =
                                    MyAccountPage.update myAccountMsg myAccountModel
                            in
                            ( { model | state = ViewingMyAccount updatedMyAccountModel }
                            , Cmd.map MyAccountMsg myAccountCommand
                            )

                        _ ->
                            ( { model | state = Problem "Problem: Received unexpected my account msg when state was not in viewing my account page." }
                            , Cmd.none
                            )

                ReviseMsg reviseMsg ->
                    case model.state of
                        ViewingRevise reviseModel ->
                            let
                                ( updatedReviseModel, reviseCommand ) =
                                    RevisePage.update reviseMsg reviseModel
                            in
                            ( { model | state = ViewingRevise updatedReviseModel }
                            , Cmd.map ReviseMsg reviseCommand
                            )

                        _ ->
                            ( { model | state = Problem "Problem: Received unexpected revise msg when state was not in viewing revision page." }
                            , Cmd.none
                            )

                LogInSuccessful _ ->
                    ( { model | state = Problem "Already logged in!" }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view { state, environment } =
    { title = "Flashcards app"
    , body =
        [ case state of
            ViewingLogin _ ->
                Element.layout [] <| viewToDisplay state

            _ ->
                Element.layout
                    [ case state of
                        ViewingManageFlashcards _ ->
                            Element.inFront (toolbarElement environment <| Just ManageFlashcards)

                        _ ->
                            Element.inFront <| toolbarElement environment Nothing
                    ]
                <|
                    el
                        [ paddingEach
                            { edges
                                | top = 50
                            }
                        , width fill
                        ]
                        (viewToDisplay state)
        ]
    }


viewToDisplay : State -> Element Msg
viewToDisplay state =
    case state of
        ViewingManageFlashcards model ->
            Element.map
                (\message -> ManageFlashcardsMsg message)
            <|
                ManageFlashcardsPage.view model

        ViewingLogin model ->
            Element.map
                (\message -> LoginMsg message)
            <|
                LoginPage.view model

        ViewingRevise model ->
            Element.map
                (\message -> ReviseMsg message)
            <|
                RevisePage.view model

        ViewingMyAccount model ->
            Element.map
                (\message -> MyAccountMsg message)
            <|
                MyAccountPage.view model

        Problem error ->
            text error



-- INIT


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { idToken, accessToken, environment } url key =
    let
        cred =
            idToken
                |> Maybe.andThen
                    (\idTokenStr ->
                        accessToken
                            |> Maybe.andThen
                                (\accessTokenStr ->
                                    Just
                                        { idToken = idTokenStr
                                        , accessToken = accessTokenStr
                                        }
                                )
                    )

        ( initialState, initialCommand ) =
            case cred of
                Nothing ->
                    ( ViewingLogin LoginPage.initialModel, Cmd.none )

                Just c ->
                    case routeFromUrl url of
                        ManageFlashcards ->
                            let
                                ( state, command ) =
                                    ManageFlashcardsPage.initialModel c.idToken
                            in
                            ( ViewingManageFlashcards state
                            , Cmd.map ManageFlashcardsMsg command
                            )

                        Revise ->
                            ( ViewingRevise <| RevisePage.initialModel, Cmd.none )

                        MyAccount ->
                            ( ViewingMyAccount <| MyAccountPage.initialModel, Cmd.none )

                        PageNotFound ->
                            ( pageNotFoundProblemState, Cmd.none )
    in
    ( { cred = cred
      , state = initialState
      , navKey = key
      , environment = Env.fromString environment
      }
    , initialCommand
    )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ActivatedLink
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map LoginMsg <| LoginPage.subscriptions
        , logInSuccessful <| LogInSuccessful << Decode.decodeValue credentialsDecoder
        ]
