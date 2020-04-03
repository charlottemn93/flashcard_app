module Credentials exposing (Credentials, credentialsDecoder)

import Json.Decode as Decode exposing (Decoder, field, string)


type alias Credentials =
    { idToken : String
    , accessToken : String
    }


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    Decode.map2 Credentials
        (field "idToken" string)
        (field "accessToken" string)
