module Environment exposing (Environment(..), fromString)


type Environment
    = Production
    | Local


fromString : String -> Environment
fromString str =
    if str == "localhost:8000" then
        Local

    else
        Production
