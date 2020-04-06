module Page.Revise exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element, centerX, column, el, fill, padding, row, spacing, width)
import ElementLibrary.Elements exposing (flashcard, heading, nextButton, shuffleButton)
import Flashcard exposing (Flashcard)
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Task



-- MODEL


type FlashcardPart
    = Word
    | Definition


type Model
    = NoFlashCards
    | ShowingFlashcard FlashcardPart Flashcard (List Flashcard) Mode


type Mode
    = Shuffle
    | MostRecent


initialModel : Model
initialModel =
    ShowingFlashcard
        Word
        { word = Just "DNA"
        , definition = Just "A nucleic acid that contains the genetic code."
        }
        [ { word = Just "Photosynthesis"
          , definition = Just "The process by which green plants and some other organisms use sunlight to synthesize nutrients from carbon dioxide and water."
          }
        , { word = Just "Nucleus"
          , definition = Just "The central part of most cells that contains genetic material and is enclosed in a membrane"
          }
        , { word = Just "DNA"
          , definition = Just "A nucleic acid that contains the genetic code."
          }
        ]
        MostRecent



-- UPDATE


type Msg
    = ShowNext
    | DisplayRandomFlashcard (List Flashcard)
    | ToggleFlashcard
    | ChangeMode Mode


nextFlashcard : Flashcard -> List Flashcard -> Maybe Flashcard
nextFlashcard currentFlashcard flashcards =
    flashcards
        |> List.filter (\card -> not (card == currentFlashcard))
        |> List.head


showNextFlashcard : Flashcard -> List Flashcard -> Mode -> Model
showNextFlashcard currentFlashcard flashcards mode =
    case nextFlashcard currentFlashcard flashcards of
        Nothing ->
            case List.head flashcards of
                Nothing ->
                    NoFlashCards

                Just _ ->
                    ShowingFlashcard Word currentFlashcard flashcards mode

        Just flashcard ->
            ShowingFlashcard Word flashcard flashcards mode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NoFlashCards ->
            ( model, Cmd.none )

        ShowingFlashcard flashcardPart currentFlashcard flashcards mode ->
            case msg of
                ShowNext ->
                    case mode of
                        Shuffle ->
                            ( ShowingFlashcard Word currentFlashcard flashcards mode
                            , Random.generate DisplayRandomFlashcard (shuffle flashcards)
                            )

                        MostRecent ->
                            ( showNextFlashcard currentFlashcard flashcards mode
                            , Cmd.none
                            )

                DisplayRandomFlashcard shuffledFlashcards ->
                    ( showNextFlashcard currentFlashcard shuffledFlashcards mode, Cmd.none )

                ToggleFlashcard ->
                    case flashcardPart of
                        Word ->
                            ( ShowingFlashcard Definition currentFlashcard flashcards mode, Cmd.none )

                        Definition ->
                            ( ShowingFlashcard Word currentFlashcard flashcards mode, Cmd.none )

                ChangeMode newMode ->
                    ( ShowingFlashcard flashcardPart currentFlashcard flashcards newMode, Cmd.none )



-- VIEW


showingFlashcard : Maybe String -> List Flashcard -> Mode -> Element Msg
showingFlashcard wordOrDef flashcards mode =
    column
        [ width fill
        , centerX
        , spacing 20
        , padding 20
        ]
        [ heading "Flash 'em - the flash card app"
        , flashcard ToggleFlashcard <| Maybe.withDefault "" wordOrDef
        , row
            [ width fill
            , spacing 10
            , centerX
            ]
            [ if List.length flashcards > 1 then
                el [ centerX ] (nextButton <| Just ShowNext)

              else
                el [ centerX ] <| nextButton Nothing
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
        ShowingFlashcard flashCardPart { word, definition } flashcards mode ->
            case flashCardPart of
                Word ->
                    showingFlashcard word flashcards mode

                Definition ->
                    showingFlashcard definition flashcards mode

        NoFlashCards ->
            column
                [ width fill
                , centerX
                , spacing 20
                , padding 20
                ]
                [ heading "No Flash Cards Remain - Add one by clicking the pencil icon!"
                ]
