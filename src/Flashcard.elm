module Flashcard exposing (Flashcard, add, flashcardDecoder, flashcardsDecoder, fromIndex, fromList, next, orderByMostRecent, previous)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Time exposing (Posix, millisToPosix, posixToMillis)


type alias Flashcard =
    { id : String
    , word : String
    , definition : String
    , createdDateTime : Posix
    }



-- if firstIndex changes, the logic for next, previous and fromList will also need to change


firstIndex : Int
firstIndex =
    1


fromList : List Flashcard -> Dict Int Flashcard
fromList flashcards =
    flashcards
        |> List.indexedMap (\i f -> ( i + 1, f ))
        |> Dict.fromList


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
    Decode.map4 Flashcard
        (Decode.field "flashCardId" string)
        (Decode.field "word" string)
        (Decode.field "definition" string)
        (Decode.field "createdDateTime" posixDecoder)


flashcardsDecoder : Decoder (List Flashcard)
flashcardsDecoder =
    list flashcardDecoder
