module Route exposing (Route(..), routeFromUrl)

-- might not need this module

import Url exposing (Url)


type Route
    = ManageFlashcards


routeFromUrl : Url -> Route
routeFromUrl _ =
    ManageFlashcards
