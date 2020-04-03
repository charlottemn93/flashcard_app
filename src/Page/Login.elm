module Page.Login exposing (Model, Msg, initialModel, subscriptions, update, view)

import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, row, spacing, width)
import ElementLibrary.Elements exposing (errorMessage, heading, inputField, passwordInputField, primaryButton, successfulMessage)
import ElementLibrary.Style exposing (HeadingLevel(..), edges)
import Json.Decode as Decode
import Ports exposing (attemptLogIn, logInFailed, signup, signupFailed, signupSuccess)



-- MODEL


type alias LoginDetails =
    { emailAddressOrUsername : String
    , password : String
    }


type alias SignupDetails =
    { username : String
    , emailAddress : String
    , password : String
    , passwordConfirmation : String
    , phoneNumber : String
    }


type Model
    = LoggedOut LoginDetails SignupDetails (Maybe String)
    | SignedUp String


initialModel : Model
initialModel =
    LoggedOut
        { emailAddressOrUsername = ""
        , password = ""
        }
        { username = ""
        , emailAddress = ""
        , password = ""
        , passwordConfirmation = ""
        , phoneNumber = ""
        }
        Nothing



-- UPDATE


type Msg
    = AttemptLogIn
    | LogInAttemptFailed (Result Decode.Error String)
    | UpdateField Field
    | SignUp
    | SignupSuccessful (Result Decode.Error String)
    | SignupFailed (Result Decode.Error String)


type Field
    = EmailAddressOrUsername String
    | Password String
    | Username String
    | EmailAddress String
    | SignupPassword String
    | SignupPasswordConfirmation String
    | PhoneNumber String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoggedOut loginDetails signupDetails errorMessage ->
            case msg of
                AttemptLogIn ->
                    ( model, attemptLogIn loginDetails )

                UpdateField (Password password) ->
                    ( LoggedOut { loginDetails | password = password } signupDetails errorMessage, Cmd.none )

                UpdateField (EmailAddressOrUsername emailAddressOrUsername) ->
                    ( LoggedOut { loginDetails | emailAddressOrUsername = emailAddressOrUsername } signupDetails errorMessage
                    , Cmd.none
                    )

                UpdateField (Username username) ->
                    ( LoggedOut loginDetails { signupDetails | username = username } errorMessage
                    , Cmd.none
                    )

                UpdateField (EmailAddress emailAddress) ->
                    ( LoggedOut loginDetails { signupDetails | emailAddress = emailAddress } errorMessage
                    , Cmd.none
                    )

                UpdateField (SignupPassword password) ->
                    ( LoggedOut loginDetails { signupDetails | password = password } errorMessage
                    , Cmd.none
                    )

                UpdateField (SignupPasswordConfirmation passwordConfirmation) ->
                    ( LoggedOut loginDetails { signupDetails | passwordConfirmation = passwordConfirmation } errorMessage
                    , Cmd.none
                    )

                UpdateField (PhoneNumber phoneNumber) ->
                    ( LoggedOut loginDetails { signupDetails | phoneNumber = phoneNumber } errorMessage
                    , Cmd.none
                    )

                LogInAttemptFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok newErrorMessage ->
                            ( LoggedOut loginDetails signupDetails (Just newErrorMessage), Cmd.none )

                        Err error ->
                            ( LoggedOut loginDetails signupDetails (Just <| "Something went wrong failing the log in attempt. Problem: " ++ Decode.errorToString error), Cmd.none )

                SignUp ->
                    if anySignUpFieldsAreEmpty signupDetails then
                        ( LoggedOut loginDetails signupDetails (Just <| "Cannot sign up - some required fields are empty."), Cmd.none )

                    else if not <| passwordsMatch signupDetails then
                        ( LoggedOut loginDetails signupDetails (Just <| "Passwords don't match"), Cmd.none )

                    else
                        ( model
                        , signup
                            { username = signupDetails.username
                            , emailAddress = signupDetails.emailAddress
                            , phoneNumber = signupDetails.phoneNumber
                            , password = signupDetails.password
                            }
                        )

                SignupSuccessful (Ok username) ->
                    ( SignedUp <| "Sign up successful! Please verify your account via SMS, " ++ username, Cmd.none )

                SignupSuccessful (Err error) ->
                    ( LoggedOut loginDetails signupDetails (Just <| "Sign up successful, but failed to get response. Problem: " ++ Decode.errorToString error), Cmd.none )

                SignupFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok errorString ->
                            ( LoggedOut loginDetails signupDetails (Just errorString), Cmd.none )

                        Err error ->
                            ( LoggedOut loginDetails signupDetails (Just <| "Sign up failed. Failed to decode error message. Problem: " ++ Decode.errorToString error), Cmd.none )

        SignedUp _ ->
            ( model, Cmd.none )


passwordsMatch : SignupDetails -> Bool
passwordsMatch { password, passwordConfirmation } =
    password == passwordConfirmation


anySignUpFieldsAreEmpty : SignupDetails -> Bool
anySignUpFieldsAreEmpty { username, emailAddress, password, passwordConfirmation, phoneNumber } =
    String.isEmpty username
        || String.isEmpty emailAddress
        || String.isEmpty password
        || String.isEmpty passwordConfirmation
        || String.isEmpty phoneNumber



-- VIEW


loginOptionsView : LoginDetails -> SignupDetails -> Element Msg
loginOptionsView { emailAddressOrUsername, password } signupDetails =
    column
        [ width fill
        , height fill
        , spacing 20
        , padding 20
        ]
        [ row
            [ width fill
            , spacing 20
            , alignTop
            ]
            [ inputField
                { fieldTitle = "Username or email address"
                , messageOnChange = \str -> UpdateField <| EmailAddressOrUsername str
                , fieldValue = emailAddressOrUsername
                , required = False
                }
            , passwordInputField
                { fieldTitle = "Password"
                , messageOnChange = \str -> UpdateField <| Password str
                , fieldValue = password
                , required = False
                }
            , el [ paddingEach { edges | top = 30 } ] <| primaryButton "Log in" <| Just AttemptLogIn
            ]
        , column
            [ centerX
            , centerY
            , width fill
            , padding 100
            ]
            [ heading One "Flashcard app!"
            , inputField
                { fieldTitle = "Username"
                , messageOnChange = \str -> UpdateField <| Username str
                , fieldValue = signupDetails.username
                , required = True
                }
            , inputField
                { fieldTitle = "Email address"
                , messageOnChange = \str -> UpdateField <| EmailAddress str
                , fieldValue = signupDetails.emailAddress
                , required = True
                }
            , passwordInputField
                { fieldTitle = "Password"
                , messageOnChange = \str -> UpdateField <| SignupPassword str
                , fieldValue = signupDetails.password
                , required = True
                }
            , passwordInputField
                { fieldTitle = "Confirm password"
                , messageOnChange = \str -> UpdateField <| SignupPasswordConfirmation str
                , fieldValue = signupDetails.passwordConfirmation
                , required = True
                }
            , inputField
                { fieldTitle = "Phone number"
                , messageOnChange = \str -> UpdateField <| PhoneNumber str
                , fieldValue = signupDetails.phoneNumber
                , required = True
                }
            , primaryButton "Sign up" <| Just SignUp
            ]
        ]


view : Model -> Element Msg
view model =
    case model of
        LoggedOut logInDetails signupDetails errorMessageStr ->
            case errorMessageStr of
                Nothing ->
                    loginOptionsView logInDetails signupDetails

                Just errorStr ->
                    column [ width fill ]
                        [ errorMessage errorStr
                        , loginOptionsView logInDetails signupDetails
                        ]

        SignedUp signupMessage ->
            column [ width fill ]
                [ successfulMessage signupMessage
                , primaryButton "Send SMS" Nothing
                ]


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ logInFailed <| LogInAttemptFailed << Decode.decodeValue (Decode.field "errorMessage" Decode.string)
        , signupSuccess <| SignupSuccessful << Decode.decodeValue (Decode.field "username" Decode.string)
        , signupFailed <| SignupFailed << Decode.decodeValue (Decode.field "errorMessage" Decode.string)
        ]
