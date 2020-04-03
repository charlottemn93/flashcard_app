module Main exposing (Msg(..), update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Credentials exposing (Credentials)
import Element exposing (text)
import ElementLibrary.Style as Style
import Page.Login as LoginPage
import Page.ManageFlashcards as ManageFlashcardsPage
import Route exposing (Route(..), routeFromUrl)
import Url exposing (Url)



-- MODEL


type alias Flags =
    { idToken : Maybe String
    , accessToken : Maybe String
    }


type alias Model =
    { cred : Maybe Credentials
    , state : State
    , navKey : Nav.Key
    }


type State
    = ViewingManageFlashcards ManageFlashcardsPage.Model
    | ViewingLogin LoginPage.Model
    | Problem String



-- UPDATE


type Msg
    = ChangedUrl Url
    | ManageFlashcardsMsg ManageFlashcardsPage.Msg
    | ActivatedLink Browser.UrlRequest
    | LoginMsg LoginPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.cred of
        Nothing ->
            case msg of
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

        Just _ ->
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
                                ( manageFlashcardsModel, manageFlashcardsCommand ) =
                                    ManageFlashcardsPage.initialModel
                            in
                            ( { model | state = ViewingManageFlashcards manageFlashcardsModel }
                            , Cmd.map ManageFlashcardsMsg manageFlashcardsCommand
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



-- VIEW


view : Model -> Document Msg
view { state } =
    { title = "Flashcards app"
    , body =
        [ Element.layout Style.globalLayout
            (case state of
                ViewingManageFlashcards model ->
                    ManageFlashcardsPage.view model

                ViewingLogin model ->
                    Element.map
                        (\message -> LoginMsg message)
                    <|
                        LoginPage.view model

                Problem error ->
                    text error
            )
        ]
    }



-- INIT


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { idToken, accessToken } url key =
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

                Just _ ->
                    case routeFromUrl url of
                        ManageFlashcards ->
                            let
                                ( manageFlashcardsModel, manageFlashcardsCommand ) =
                                    ManageFlashcardsPage.initialModel
                            in
                            ( ViewingManageFlashcards manageFlashcardsModel, Cmd.map ManageFlashcardsMsg manageFlashcardsCommand )
    in
    ( { cred = cred
      , state = initialState
      , navKey = key
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
    Sub.map LoginMsg <| LoginPage.subscriptions
