module Route exposing (Route(..), hrefString, routeFromUrl)

import Environment exposing (Environment(..))
import Url exposing (Url)


type Route
    = ManageFlashcards
    | Revise
    | MyAccount
    | PageNotFound


host : Environment -> String
host env =
    case env of
        Local ->
            "http://localhost:8000"

        Production ->
            "http://flashcardapp-charlotte-neill.s3-website.eu-west-2.amazonaws.com"


routeFromUrl : Url -> Route
routeFromUrl { path } =
    if path == "/manage-flashcards" then
        ManageFlashcards

    else if path == "/revise" then
        Revise

    else if path == "/my-account" then
        MyAccount

    else
        PageNotFound


hrefString : Environment -> Route -> String
hrefString env route =
    case route of
        ManageFlashcards ->
            host env ++ "/manage-flashcards"

        Revise ->
            host env ++ "/revise"

        MyAccount ->
            host env ++ "/my-account"

        PageNotFound ->
            host env ++ "/404"
