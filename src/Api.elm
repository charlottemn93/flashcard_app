module Api exposing (editFlashcardRequest, httpError, loadFlashcardsRequest, saveFlashcardRequest)

import Flashcard exposing (Flashcard, flashcardDecoder, flashcardsDecoder)
import Http as Http exposing (Error(..))
import HttpBuilder exposing (get, post, request, withExpect, withHeaders, withJsonBody)
import Json.Encode as Encode
import Time exposing (Posix, posixToMillis)


httpError : Http.Error -> String
httpError error =
    case error of
        BadUrl str ->
            "Bad url " ++ str

        Timeout ->
            "Network request timed out"

        NetworkError ->
            "Network error - please make sure you're connected to the internet"

        BadStatus int ->
            "Bad status " ++ String.fromInt int

        BadBody str ->
            "Bad body " ++ str


saveFlashcardRequest : { word : String, definition : String, idToken : String, posix : Posix, expectMsg : Result Http.Error Flashcard -> msg } -> Cmd msg
saveFlashcardRequest { word, definition, idToken, posix, expectMsg } =
    request
        (post "https://unq1uv1ab5.execute-api.eu-west-2.amazonaws.com/prod/flashcard"
            |> withHeaders
                [ ( "Content-Type", "application/json" )
                , ( "Authorization", idToken )
                ]
            |> withJsonBody
                (Encode.object
                    [ ( "createdDateTime", Encode.int <| posixToMillis posix )
                    , ( "word", Encode.string word )
                    , ( "definition", Encode.string definition )
                    ]
                )
            |> withExpect (Http.expectJson expectMsg flashcardDecoder)
        )


editFlashcardRequest : { id : String, word : String, definition : String, idToken : String, posix : Posix, expectMsg : Result Http.Error Flashcard -> msg } -> Cmd msg
editFlashcardRequest { id, word, definition, idToken, posix, expectMsg } =
    request
        (post ("https://unq1uv1ab5.execute-api.eu-west-2.amazonaws.com/prod/flashcard/" ++ id)
            |> withHeaders
                [ ( "Content-Type", "application/json" )
                , ( "Authorization", idToken )
                ]
            |> withJsonBody
                (Encode.object
                    [ ( "createdDateTime", Encode.int <| posixToMillis posix )
                    , ( "word", Encode.string word )
                    , ( "definition", Encode.string definition )
                    , ( "flashcardId", Encode.string id )
                    ]
                )
            |> withExpect (Http.expectJson expectMsg flashcardDecoder)
        )


loadFlashcardsRequest : { idToken : String, expectMsg : Result Http.Error (List Flashcard) -> msg } -> Cmd msg
loadFlashcardsRequest { idToken, expectMsg } =
    request
        (get "https://unq1uv1ab5.execute-api.eu-west-2.amazonaws.com/prod/flashcards"
            |> withHeaders
                [ ( "Authorization", idToken ) ]
            |> withExpect (Http.expectJson expectMsg flashcardsDecoder)
        )
