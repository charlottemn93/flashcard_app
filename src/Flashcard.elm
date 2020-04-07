module Flashcard exposing (Flashcard, fromIndex)

import Dict exposing (Dict)
import Time exposing (Posix)


type alias Flashcard =
    { word : Maybe String
    , definition : Maybe String
    , createdDateTime : Posix
    }


fromIndex : Int -> Dict Int Flashcard -> Maybe Flashcard
fromIndex index flashcards =
    flashcards |> Dict.get index
