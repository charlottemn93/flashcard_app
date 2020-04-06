module Page.Revise exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element, centerX, column, fill, padding, row, spacing, width)
import ElementLibrary.Elements exposing (dangerButton, errorMessage, flashcard, heading, inputField, primaryButton)
import ElementLibrary.Style exposing (HeadingLevel(..))
import Flashcard exposing (Flashcard)
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Task



-- MODEL


type Result
    = Ok Flashcard
    | Err String


type FlashcardPart
    = Word
    | Definition


type Model
    = AddFlashcard Flashcard (List Flashcard) (Maybe String)
    | NoFlashCardsRemain
    | Show FlashcardPart Flashcard (List Flashcard)
    | Problem String


initialModel : Model
initialModel =
    Show
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



-- UPDATE


type Msg
    = ShowNext
    | DisplayRandomFlashcard (List Flashcard)
    | ToggleFlashcard
    | DeleteFlashcard
    | HandleAdd
    | UpdateWord String
    | UpdateDefinition String
    | SaveFlashcard Flashcard (List Flashcard)


saveFlashCard : Flashcard -> Result
saveFlashCard { word, definition } =
    case ( word, definition ) of
        ( Just updatedWord, Just def ) ->
            Ok
                { word = Just updatedWord
                , definition = Just def
                }

        ( Nothing, Nothing ) ->
            Err "Word and definition empty! Please enter a word and a definition."

        ( Nothing, _ ) ->
            Err "Word empty! Please enter a word."

        ( _, Nothing ) ->
            Err "Definition empty! Please enter a definition."


nextFlashcard : Flashcard -> List Flashcard -> Maybe Flashcard
nextFlashcard currentFlashcard flashcards =
    flashcards
        |> List.filter (\card -> not (card == currentFlashcard))
        |> List.head


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        AddFlashcard flashcardDetails flashcards errorString ->
            case msg of
                UpdateWord str ->
                    ( AddFlashcard
                        { flashcardDetails
                            | word =
                                if String.isEmpty str then
                                    Nothing

                                else
                                    Just str
                        }
                        flashcards
                        errorString
                    , Cmd.none
                    )

                UpdateDefinition str ->
                    ( AddFlashcard
                        { flashcardDetails
                            | definition =
                                if String.isEmpty str then
                                    Nothing

                                else
                                    Just str
                        }
                        flashcards
                        errorString
                    , Cmd.none
                    )

                SaveFlashcard { word, definition } currentFlashcardList ->
                    let
                        newFlashcard =
                            saveFlashCard { word = word, definition = definition }
                    in
                    case newFlashcard of
                        Ok flashCard ->
                            ( Show Word flashCard (currentFlashcardList ++ [ flashCard ]), Cmd.none )

                        Err errorMsg ->
                            ( AddFlashcard flashcardDetails flashcards (Just errorMsg), Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'adding a flashcard'. Please refresh the browser, if this persists, contact the adminstrator.", Cmd.none )

        Show flashcardPart currentFlashcard flashcards ->
            case msg of
                ShowNext ->
                    ( Show Word currentFlashcard flashcards
                    , Random.generate DisplayRandomFlashcard (shuffle flashcards)
                    )

                DisplayRandomFlashcard shuffledFlashcards ->
                    case nextFlashcard currentFlashcard shuffledFlashcards of
                        Nothing ->
                            case List.head shuffledFlashcards of
                                Nothing ->
                                    ( NoFlashCardsRemain, Cmd.none )

                                Just _ ->
                                    ( Show Word currentFlashcard shuffledFlashcards, Cmd.none )

                        Just flashcard ->
                            ( Show Word flashcard shuffledFlashcards, Cmd.none )

                DeleteFlashcard ->
                    let
                        filteredFlashcards =
                            List.filter (\f -> f /= currentFlashcard) flashcards
                    in
                    case List.head filteredFlashcards of
                        Just flashcard ->
                            ( Show Word flashcard filteredFlashcards
                            , Random.generate DisplayRandomFlashcard (shuffle filteredFlashcards)
                            )

                        Nothing ->
                            ( NoFlashCardsRemain, Cmd.none )

                HandleAdd ->
                    ( AddFlashcard { word = Nothing, definition = Nothing } flashcards Nothing, Cmd.none )

                ToggleFlashcard ->
                    case flashcardPart of
                        Word ->
                            ( Show Definition currentFlashcard flashcards, Cmd.none )

                        Definition ->
                            ( Show Word currentFlashcard flashcards, Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'showing flashcard part'. Please refresh the browser, if this persists, contact the adminstrator.", Cmd.none )

        NoFlashCardsRemain ->
            case msg of
                HandleAdd ->
                    ( AddFlashcard { word = Nothing, definition = Nothing } [] Nothing, Cmd.none )

                _ ->
                    ( NoFlashCardsRemain, Cmd.none )

        Problem _ ->
            ( model, Cmd.none )



-- VIEW


addingFlashcardView : Flashcard -> List Flashcard -> Maybe String -> Element Msg
addingFlashcardView { word, definition } flashcards errorString =
    column
        [ width fill ]
        [ case errorString of
            Nothing ->
                Element.none

            Just str ->
                errorMessage str
        , column
            [ width fill
            , centerX
            , spacing 20
            , padding 20
            ]
            [ heading One "Flash 'em - the flash card app"
            , inputField
                { fieldTitle = "Word"
                , fieldValue = Maybe.withDefault "" word
                , messageOnChange = UpdateWord
                , required = True
                }
            , inputField
                { fieldTitle = "Definition"
                , fieldValue = Maybe.withDefault "" definition
                , messageOnChange = UpdateDefinition
                , required = True
                }
            , primaryButton "Save" <| Just (SaveFlashcard { word = word, definition = definition } flashcards)
            ]
        ]


showingFlashcard : Maybe String -> List Flashcard -> Element Msg
showingFlashcard wordOrDef flashcards =
    column
        [ width fill
        , centerX
        , spacing 20
        , padding 20
        ]
        [ heading One "Flash 'em - the flash card app"
        , flashcard ToggleFlashcard <| Maybe.withDefault "" wordOrDef
        , row
            [ width fill
            , spacing 10
            ]
            [ if List.length flashcards > 1 then
                primaryButton "Next" <| Just ShowNext

              else
                primaryButton "Next" Nothing
            , primaryButton "Add" <| Just HandleAdd
            , dangerButton "Delete" <| Just DeleteFlashcard
            ]
        ]


view : Model -> Element Msg
view model =
    case model of
        AddFlashcard flashcard flashcards errorString ->
            addingFlashcardView flashcard flashcards errorString

        Show flashCardPart { word, definition } flashcards ->
            case flashCardPart of
                Word ->
                    showingFlashcard word flashcards

                Definition ->
                    showingFlashcard definition flashcards

        NoFlashCardsRemain ->
            column
                [ width fill
                , centerX
                , spacing 20
                , padding 20
                ]
                [ heading One "No Flash Cards Remain - Add one!"
                , primaryButton "Add" <| Just HandleAdd
                ]

        Problem errorString ->
            column [ width fill ]
                [ errorMessage errorString ]
