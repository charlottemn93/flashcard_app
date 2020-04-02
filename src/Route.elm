module Route exposing (Route(..), routeFromUrl)

import Url exposing (Url)


type Route
    = ManageFlashcards


routeFromUrl : Url -> Route
routeFromUrl _ =
    ManageFlashcards
