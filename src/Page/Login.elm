module Page.Login exposing (Model, Msg, initialModel, subscriptions, update, view)

import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, row, spacing, width)
import ElementLibrary.Elements exposing (Message, button, heading, inputField, message, passwordInputField)
import ElementLibrary.Helpers exposing (MessageType(..), edges)
import Json.Decode as Decode
import Ports.Login exposing (attemptLogIn, logInFailed, signup, signupFailed, signupSuccess, verificationFailed, verificationSuccessful, verifyAccount)



-- MODEL


type alias LoginDetails =
    { username : String
    , password : String
    }


type alias SignupDetails =
    { username : String
    , emailAddress : String
    , password : String
    , passwordConfirmation : String
    }


type Model
    = LoggedOut LoginDetails SignupDetails (Maybe Message)
    | SignedUp { verificationCode : String, username : String, errorString : Maybe String }


initialModel : Model
initialModel =
    LoggedOut
        initialLoginDetails
        initialSignupDetails
        Nothing


initialLoginDetails : LoginDetails
initialLoginDetails =
    { username = ""
    , password = ""
    }


initialSignupDetails : SignupDetails
initialSignupDetails =
    { username = ""
    , emailAddress = ""
    , password = ""
    , passwordConfirmation = ""
    }



-- UPDATE


type Msg
    = AttemptLogIn
    | LogInAttemptFailed (Result Decode.Error String)
    | UpdateField Field
    | SignUp
    | SignupSuccessful
    | SignupFailed (Result Decode.Error String)
    | VerifyAccount
    | VerificationSuccessful
    | VerificationFailed (Result Decode.Error String)


type Field
    = Username String
    | Password String
    | SignupUsername String
    | EmailAddress String
    | SignupPassword String
    | SignupPasswordConfirmation String
    | VerificationCode String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoggedOut loginDetails signupDetails errorMessage ->
            case msg of
                AttemptLogIn ->
                    ( model, attemptLogIn loginDetails )

                UpdateField (Password password) ->
                    ( LoggedOut { loginDetails | password = password } signupDetails errorMessage, Cmd.none )

                UpdateField (Username username) ->
                    ( LoggedOut { loginDetails | username = username } signupDetails errorMessage
                    , Cmd.none
                    )

                UpdateField (SignupUsername username) ->
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

                LogInAttemptFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok newErrorMessage ->
                            ( LoggedOut loginDetails
                                signupDetails
                              <|
                                Just
                                    { messageString = newErrorMessage
                                    , messageType = Error
                                    }
                            , Cmd.none
                            )

                        Err error ->
                            ( LoggedOut loginDetails signupDetails <|
                                Just
                                    { messageString = "Something went wrong failing the log in attempt. Problem: " ++ Decode.errorToString error
                                    , messageType = Error
                                    }
                            , Cmd.none
                            )

                SignUp ->
                    if anySignUpFieldsAreEmpty signupDetails then
                        ( LoggedOut loginDetails signupDetails <|
                            Just
                                { messageString = "Cannot sign up - some required fields are empty."
                                , messageType = Error
                                }
                        , Cmd.none
                        )

                    else if not <| passwordsMatch signupDetails then
                        ( LoggedOut loginDetails signupDetails <|
                            Just
                                { messageString = "Passwords don't match"
                                , messageType = Error
                                }
                        , Cmd.none
                        )

                    else
                        ( model
                        , signup
                            { username = signupDetails.username
                            , emailAddress = signupDetails.emailAddress
                            , password = signupDetails.password
                            }
                        )

                SignupSuccessful ->
                    ( SignedUp { verificationCode = "", errorString = Nothing, username = signupDetails.username }, Cmd.none )

                SignupFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok errorString ->
                            ( LoggedOut loginDetails signupDetails <|
                                Just
                                    { messageString = errorString
                                    , messageType = Error
                                    }
                            , Cmd.none
                            )

                        Err error ->
                            ( LoggedOut loginDetails signupDetails <|
                                Just
                                    { messageString = "Sign up failed. Failed to decode error message. Problem: " ++ Decode.errorToString error
                                    , messageType = Error
                                    }
                            , Cmd.none
                            )

                _ ->
                    ( LoggedOut loginDetails signupDetails <|
                        Just
                            { messageString = "Unexpected msg received in logged out state."
                            , messageType = Error
                            }
                    , Cmd.none
                    )

        SignedUp details ->
            case msg of
                VerifyAccount ->
                    ( model
                    , verifyAccount
                        { username = details.username
                        , verificationCode = details.verificationCode
                        }
                    )

                UpdateField (VerificationCode code) ->
                    ( SignedUp { details | verificationCode = code }, Cmd.none )

                VerificationSuccessful ->
                    ( LoggedOut initialLoginDetails
                        initialSignupDetails
                      <|
                        Just
                            { messageString = "Verification successful! Please log in."
                            , messageType = Successful
                            }
                    , Cmd.none
                    )

                VerificationFailed result ->
                    case result of
                        -- get the more specific error message returned from the port, or display the reason the string decoder failed to the developer
                        Ok errorString ->
                            ( SignedUp { details | errorString = Just errorString }, Cmd.none )

                        Err error ->
                            ( SignedUp { details | errorString = Just <| "Verification failed. Failed to decode error message. Problem: " ++ Decode.errorToString error }, Cmd.none )

                _ ->
                    ( SignedUp { details | errorString = Just "Unexpected msg received in signed up state." }, Cmd.none )


passwordsMatch : SignupDetails -> Bool
passwordsMatch { password, passwordConfirmation } =
    password == passwordConfirmation


anySignUpFieldsAreEmpty : SignupDetails -> Bool
anySignUpFieldsAreEmpty { username, emailAddress, password, passwordConfirmation } =
    String.isEmpty username
        || String.isEmpty emailAddress
        || String.isEmpty password
        || String.isEmpty passwordConfirmation



-- VIEW


loginOptionsView : LoginDetails -> SignupDetails -> Element Msg
loginOptionsView { username, password } signupDetails =
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
                { fieldTitle = "Username"
                , messageOnChange = \str -> UpdateField <| Username str
                , fieldValue = username
                }
            , passwordInputField
                { fieldTitle = "Password"
                , messageOnChange = \str -> UpdateField <| Password str
                , fieldValue = password
                }
            , el [ paddingEach { edges | top = 30 } ] <| button "Log in" <| Just AttemptLogIn
            ]
        , column
            [ centerX
            , centerY
            , width fill
            , padding 100
            ]
            [ heading "Flash 'em - the flash card app"
            , inputField
                { fieldTitle = "Username"
                , messageOnChange = \str -> UpdateField <| SignupUsername str
                , fieldValue = signupDetails.username
                }
            , inputField
                { fieldTitle = "Email address"
                , messageOnChange = \str -> UpdateField <| EmailAddress str
                , fieldValue = signupDetails.emailAddress
                }
            , passwordInputField
                { fieldTitle = "Password"
                , messageOnChange = \str -> UpdateField <| SignupPassword str
                , fieldValue = signupDetails.password
                }
            , passwordInputField
                { fieldTitle = "Confirm password"
                , messageOnChange = \str -> UpdateField <| SignupPasswordConfirmation str
                , fieldValue = signupDetails.passwordConfirmation
                }
            , button "Sign up" <| Just SignUp
            ]
        ]


view : Model -> Element Msg
view model =
    case model of
        LoggedOut logInDetails signupDetails errorMessageStr ->
            case errorMessageStr of
                Nothing ->
                    loginOptionsView logInDetails signupDetails

                Just { messageString, messageType } ->
                    column [ width fill ]
                        [ message
                            { messageType = messageType
                            , messageString = messageString
                            }
                        , loginOptionsView logInDetails signupDetails
                        ]

        SignedUp { verificationCode, errorString } ->
            column [ width fill ]
                [ case errorString of
                    Nothing ->
                        Element.none

                    Just str ->
                        message
                            { messageType = Error
                            , messageString = str
                            }
                , column
                    [ width fill
                    , padding 20
                    , spacing 10
                    ]
                    [ message
                        { messageType = Successful
                        , messageString = "Sign up successful! A verification code will be sent to your email address in a few minutes."
                        }
                    , inputField
                        { fieldTitle = "Verification code"
                        , messageOnChange = \str -> UpdateField <| VerificationCode str
                        , fieldValue = verificationCode
                        }
                    , button "Verify account" <| Just VerifyAccount
                    ]
                ]


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ logInFailed <| LogInAttemptFailed << Decode.decodeValue (Decode.field "errorMessage" Decode.string)
        , signupSuccess <| \_ -> SignupSuccessful
        , signupFailed <| SignupFailed << Decode.decodeValue (Decode.field "errorMessage" Decode.string)
        , verificationFailed <| VerificationFailed << Decode.decodeValue (Decode.field "errorMessage" Decode.string)
        , verificationSuccessful (\_ -> VerificationSuccessful)
        ]
