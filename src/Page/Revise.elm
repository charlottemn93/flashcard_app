module Page.Revise exposing (Model, Msg, initialModel, update, view)

import Api exposing (httpError, loadFlashcardsRequest)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, paragraph, row, spacing, text, width)
import ElementLibrary.Elements exposing (Message, buttonImage, flashcard, heading, message, shuffleButton)
import ElementLibrary.Helpers exposing (MessageType(..))
import Flashcard as Flashcard exposing (Flashcard)
import Http as Http
import Random
import Random.List exposing (shuffle)



-- MODEL


type FlashcardPart
    = Word
    | Definition


type alias Model =
    { state : State
    , idToken : String
    }


type State
    = ShowingFlashcard
        { flashcardPart : FlashcardPart
        , currentFlashcardIndex : Int
        , flashcards : Dict Int Flashcard
        , mode : Mode
        }
    | NoFlashcardsToShow (Maybe String)


type Mode
    = Shuffle
    | MostRecent


initialModel : String -> ( Model, Cmd Msg )
initialModel idToken =
    ( { idToken = idToken
      , state = NoFlashcardsToShow Nothing
      }
    , loadFlashcardsRequest
        { idToken = idToken
        , expectMsg = LoadFlashcards
        }
    )



-- UPDATE


type Msg
    = ShowNext
    | ShowPrevious
    | CreatedRandomDeck (List Int)
    | ToggleFlashcard
    | ChangeMode Mode
    | LoadFlashcards (Result Http.Error (List Flashcard))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( state, command ) =
            updateState msg model
    in
    ( { model | state = state }, command )


updateState : Msg -> Model -> ( State, Cmd Msg )
updateState msg { state } =
    case state of
        NoFlashcardsToShow _ ->
            case msg of
                LoadFlashcards (Ok flashcards) ->
                    ( ShowingFlashcard
                        { flashcardPart = Word
                        , currentFlashcardIndex = Flashcard.firstIndex
                        , flashcards = Flashcard.fromList flashcards
                        , mode = MostRecent
                        }
                    , Cmd.none
                    )

                LoadFlashcards (Err e) ->
                    ( NoFlashcardsToShow (Just <| httpError e), Cmd.none )

                _ ->
                    ( state, Cmd.none )

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

                LoadFlashcards _ ->
                    ( state, Cmd.none )



-- VIEW


showingFlashcard : String -> Dict Int Flashcard -> Mode -> Element Msg
showingFlashcard wordOrDef flashcards mode =
    column
        [ width fill
        , centerX
        , spacing 20
        , padding 20
        ]
        [ heading "Flash card app"
        , flashcard
            { onClickMsg = ToggleFlashcard
            , isEditable = False
            , label = paragraph [] [ text wordOrDef ]
            }
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
                    , src = "./images/icons/left-arrow.svg"
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
                    , src = "./images/icons/right-arrow.svg"
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
view { state } =
    case state of
        NoFlashcardsToShow errorString ->
            column [ width fill ]
                [ row
                    [ Element.alignTop
                    , width fill
                    ]
                    [ case errorString of
                        Nothing ->
                            Element.none

                        Just x ->
                            message
                                { messageString = x
                                , messageType = Error
                                }
                    ]
                , row
                    [ width fill
                    , centerX
                    , spacing 20
                    , padding 20
                    ]
                    [ heading "No Flash cards exist - add one by clicking the pencil icon!" ]
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
