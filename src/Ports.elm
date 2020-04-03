port module Ports exposing (attemptLogIn, logInFailed, signup, signupFailed, signupSuccess)

import Json.Decode exposing (Value)



-- OUTGOING


port attemptLogIn : { emailAddressOrUsername : String, password : String } -> Cmd msg


port signup : { emailAddress : String, password : String, username : String, phoneNumber : String } -> Cmd msg



-- INCOMING


port logInFailed : (Value -> msg) -> Sub msg


port signupFailed : (Value -> msg) -> Sub msg


port signupSuccess : (Value -> msg) -> Sub msg
