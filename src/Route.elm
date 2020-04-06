module Route exposing (Route(..), hrefString, routeFromUrl)

import Environment exposing (Environment(..))
import Url exposing (Url)


type Route
    = ManageFlashcards


host : Environment -> String
host env =
    case env of
        Local ->
            "http://localhost:8000"

        Production ->
            "http://flashcardapp-charlotte-neill.s3-website.eu-west-2.amazonaws.com"


routeFromUrl : Url -> Route
routeFromUrl _ =
    ManageFlashcards


hrefString : Environment -> Route -> String
hrefString env route =
    case route of
        ManageFlashcards ->
            host env ++ "/manage-flashcards"
