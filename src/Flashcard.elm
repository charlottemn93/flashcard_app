module Flashcard exposing (Flashcard, add, flashcardDecoder, fromIndex, next, orderByMostRecent, previous)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, string)
import Time exposing (Posix, millisToPosix, posixToMillis)


type alias Flashcard =
    { word : String
    , definition : String
    , createdDateTime : Posix
    }



-- if firstIndex changes, the logic for next and previous will also need to change


firstIndex : Int
firstIndex =
    1


next : Int -> Dict Int Flashcard -> Int
next currentFlashcardIndex flashcards =
    if currentFlashcardIndex == Dict.size flashcards then
        firstIndex

    else
        currentFlashcardIndex + 1


previous : Int -> Dict Int Flashcard -> Int
previous currentFlashcardIndex flashcards =
    if currentFlashcardIndex == firstIndex then
        Dict.size flashcards

    else
        currentFlashcardIndex - 1


orderByMostRecent : Dict Int Flashcard -> Dict Int Flashcard
orderByMostRecent flashcards =
    flashcards
        |> Dict.toList
        |> List.sortBy
            (\( _, { createdDateTime } ) ->
                posixToMillis createdDateTime
            )
        |> List.map (\( _, f ) -> f)
        |> List.indexedMap (\i f -> ( i, f ))
        |> Dict.fromList


add : Maybe (Dict Int Flashcard) -> Flashcard -> Dict Int Flashcard
add flashcards newFlashcard =
    case flashcards of
        Nothing ->
            Dict.singleton firstIndex newFlashcard

        Just cards ->
            let
                newIndex =
                    Dict.size cards + 1
            in
            cards
                |> Dict.insert newIndex newFlashcard


fromIndex : Int -> Dict Int Flashcard -> Maybe Flashcard
fromIndex index flashcards =
    flashcards |> Dict.get index


posixDecoder : Decoder Posix
posixDecoder =
    int
        |> Decode.andThen
            (\millis ->
                Decode.succeed <| millisToPosix millis
            )


flashcardDecoder : Decoder Flashcard
flashcardDecoder =
    Decode.map3 Flashcard
        (Decode.field "word" string)
        (Decode.field "definition" string)
        (Decode.field "createdDateTime" posixDecoder)
