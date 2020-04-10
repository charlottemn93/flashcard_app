module Page.Revise exposing (Model, Msg, initialModel, update, view)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, row, spacing, width)
import ElementLibrary.Elements exposing (buttonImage, flashcard, heading, message, shuffleButton)
import ElementLibrary.Helpers exposing (MessageType(..))
import Flashcard as Flashcard exposing (Flashcard)
import Random
import Random.List exposing (shuffle)



-- MODEL


type FlashcardPart
    = Word
    | Definition


type Model
    = ShowingFlashcard
        { flashcardPart : FlashcardPart
        , currentFlashcardIndex : Int
        , flashcards : Dict Int Flashcard
        , mode : Mode
        }
    | NoFlashcardsToShow


type Mode
    = Shuffle
    | MostRecent


initialModel : Model
initialModel =
    NoFlashcardsToShow



-- UPDATE


type Msg
    = ShowNext
    | ShowPrevious
    | CreatedRandomDeck (List Int)
    | ToggleFlashcard
    | ChangeMode Mode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NoFlashcardsToShow ->
            ( model, Cmd.none )

        ShowingFlashcard details ->
            case msg of
                ShowPrevious ->
                    ( ShowingFlashcard { details | currentFlashcardIndex = Flashcard.previous details.currentFlashcardIndex details.flashcards }
                    , Cmd.none
                    )

                ShowNext ->
                    ( ShowingFlashcard { details | currentFlashcardIndex = Flashcard.next details.currentFlashcardIndex details.flashcards }
                    , Cmd.none
                    )

                CreatedRandomDeck shuffledIndexes ->
                    let
                        shuffledFlashcards =
                            shuffledIndexes
                                |> List.filterMap
                                    (\i ->
                                        details.flashcards |> Dict.get i
                                    )
                                |> List.indexedMap (\i f -> ( i, f ))
                                |> Dict.fromList
                    in
                    ( ShowingFlashcard
                        { details
                            | flashcardPart = Word
                            , flashcards = shuffledFlashcards
                        }
                    , Cmd.none
                    )

                ToggleFlashcard ->
                    case details.flashcardPart of
                        Word ->
                            ( ShowingFlashcard { details | flashcardPart = Definition }
                            , Cmd.none
                            )

                        Definition ->
                            ( ShowingFlashcard { details | flashcardPart = Word }
                            , Cmd.none
                            )

                ChangeMode newMode ->
                    case newMode of
                        Shuffle ->
                            -- shuffle deck
                            ( ShowingFlashcard { details | mode = newMode }
                            , Random.generate CreatedRandomDeck
                                (shuffle <| Dict.keys details.flashcards)
                            )

                        MostRecent ->
                            ( ShowingFlashcard
                                { details
                                    | mode = newMode
                                    , flashcards = Flashcard.orderByMostRecent details.flashcards
                                }
                            , Cmd.none
                            )



-- VIEW


showingFlashcard : String -> Dict Int Flashcard -> Mode -> Element Msg
showingFlashcard wordOrDef flashcards mode =
    column
        [ width fill
        , centerX
        , spacing 20
        , padding 20
        ]
        [ heading "Flash 'em - the flash card app"
        , flashcard ToggleFlashcard wordOrDef
        , row
            [ width fill
            , spacing 10
            , centerX
            ]
            [ el [ centerX ]
                (buttonImage
                    { onClickMsg =
                        if Dict.size flashcards > 1 then
                            Just ShowPrevious

                        else
                            Nothing
                    , src = "./images/icons/previous.svg"
                    , description = "Previous"
                    , specifiedImageHeight = Nothing
                    }
                )
            , el [ centerX ]
                (buttonImage
                    { onClickMsg =
                        if Dict.size flashcards > 1 then
                            Just ShowNext

                        else
                            Nothing
                    , src = "./images/icons/next.svg"
                    , description = "Next"
                    , specifiedImageHeight = Nothing
                    }
                )
            , case mode of
                Shuffle ->
                    el [ centerX ] <| shuffleButton True (Just <| ChangeMode MostRecent)

                MostRecent ->
                    el [ centerX ] <| shuffleButton False (Just <| ChangeMode Shuffle)
            ]
        ]


view : Model -> Element Msg
view model =
    case model of
        NoFlashcardsToShow ->
            column
                [ width fill
                , centerX
                , spacing 20
                , padding 20
                ]
                [ heading "No Flash cards exist - add one by clicking the pencil icon!"
                ]

        ShowingFlashcard { flashcardPart, currentFlashcardIndex, flashcards, mode } ->
            case Flashcard.fromIndex currentFlashcardIndex flashcards of
                Nothing ->
                    column
                        [ Element.alignTop
                        , width fill
                        ]
                        [ message
                            { messageString = "Something has gone wrong. Flashcard does not exist."
                            , messageType = Error
                            }
                        ]

                Just { word, definition } ->
                    case flashcardPart of
                        Word ->
                            showingFlashcard word flashcards mode

                        Definition ->
                            showingFlashcard definition flashcards mode
