port module Ports.Login exposing (attemptLogIn, logInFailed, logInSuccessful, signup, signupFailed, signupSuccess, verificationFailed, verificationSuccessful, verifyAccount)

import Json.Decode exposing (Value)



-- OUTGOING


port attemptLogIn : { username : String, password : String } -> Cmd msg


port signup : { emailAddress : String, password : String, username : String } -> Cmd msg


port verifyAccount : { username : String, verificationCode : String } -> Cmd msg



-- INCOMING


port logInFailed : (Value -> msg) -> Sub msg


port signupFailed : (Value -> msg) -> Sub msg


port signupSuccess : (Value -> msg) -> Sub msg


port verificationSuccessful : (Value -> msg) -> Sub msg


port verificationFailed : (Value -> msg) -> Sub msg


port logInSuccessful : (Value -> msg) -> Sub msg
