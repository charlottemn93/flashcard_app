module Page.Login exposing (Model, Msg, initialModel, subscriptions, update, view)

import Element exposing (Element, column, fill, row, spacing, width)
import ElementLibrary.Elements exposing (errorMessage, inputField, primaryButton)
import Json.Decode as Decode
import Ports exposing (attemptLogIn, logInFailed)



-- MODEL


type Model
    = LoggedOut
        { emailAddressOrUsername : String
        , password : String
        }
    | Problem
        String
        { emailAddressOrUsername : String
        , password : String
        }


initialModel : Model
initialModel =
    LoggedOut { emailAddressOrUsername = "", password = "" }



-- UPDATE


type Msg
    = AttemptLogIn
    | UpdatePasswordField String
    | UpdateEmailAddressOrUsernameField String
    | LogInAttemptFailed (Result Decode.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoggedOut loginDetails ->
            case msg of
                AttemptLogIn ->
                    ( model, attemptLogIn loginDetails )

                UpdatePasswordField password ->
                    ( LoggedOut { loginDetails | password = password }, Cmd.none )

                UpdateEmailAddressOrUsernameField emailAddressOrUsername ->
                    ( LoggedOut { loginDetails | emailAddressOrUsername = emailAddressOrUsername }
                    , Cmd.none
                    )

                LogInAttemptFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok errorMessage ->
                            ( Problem errorMessage loginDetails, Cmd.none )

                        Err error ->
                            ( Problem ("Something went wrong failing the log in attempt. Problem: " ++ Decode.errorToString error) loginDetails, Cmd.none )

        Problem errorMessage loginDetails ->
            case msg of
                AttemptLogIn ->
                    ( model, attemptLogIn loginDetails )

                UpdatePasswordField password ->
                    ( Problem errorMessage { loginDetails | password = password }, Cmd.none )

                UpdateEmailAddressOrUsernameField emailAddressOrUsername ->
                    ( Problem errorMessage { loginDetails | emailAddressOrUsername = emailAddressOrUsername }
                    , Cmd.none
                    )

                LogInAttemptFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok newErrorMessage ->
                            ( Problem newErrorMessage loginDetails, Cmd.none )

                        Err error ->
                            ( Problem ("Something went wrong failing the log in attempt. Problem: " ++ Decode.errorToString error) loginDetails, Cmd.none )



-- VIEW


loginOptionsView : { emailAddressOrUsername : String, password : String } -> Element Msg
loginOptionsView { emailAddressOrUsername, password } =
    column
        [ width fill
        , spacing 20
        ]
        [ row
            [ width fill
            , spacing 20
            ]
            [ inputField
                { fieldTitle = "Username or email address"
                , messageOnChange = UpdateEmailAddressOrUsernameField
                , fieldValue = emailAddressOrUsername
                }
            , inputField
                { fieldTitle = "Password"
                , messageOnChange = UpdatePasswordField
                , fieldValue = password
                }
            , primaryButton "Log in" <| Just AttemptLogIn
            ]
        ]


view : Model -> Element Msg
view model =
    case model of
        LoggedOut logInDetails ->
            loginOptionsView logInDetails

        Problem errorMessageStr logInDetails ->
            column [ width fill ]
                [ errorMessage errorMessageStr
                , loginOptionsView logInDetails
                ]


subscriptions : Sub Msg
subscriptions =
    logInFailed <| LogInAttemptFailed << Decode.decodeValue (Decode.field "errorMessage" Decode.string)
