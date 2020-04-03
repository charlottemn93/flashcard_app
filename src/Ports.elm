port module Ports exposing (attemptLogIn, logInFailed)

import Json.Decode exposing (Value)



-- port module Ports exposing (attemptLogIn, signup, signupFailed, signupSuccess)
-- OUTGOING


port attemptLogIn : { emailAddressOrUsername : String, password : String } -> Cmd msg



-- INCOMING


port logInFailed : (Value -> msg) -> Sub msg



-- port signup : { emailAddress : String, password : String, username : String } -> Cmd msg
-- port signupFailed : (String -> msg) -> Sub msg
-- port signupSuccess : (String -> msg) -> Sub msg
