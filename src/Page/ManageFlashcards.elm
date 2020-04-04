module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element, centerX, column, fill, padding, row, spacing, width)
import ElementLibrary.Elements exposing (dangerButton, errorMessage, flashcard, heading, inputField, primaryButton)
import ElementLibrary.Style exposing (HeadingLevel(..))
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Task



---- MODEL ----


type alias Flashcard =
    { word : Maybe String
    , definition : Maybe String
    }


type Result
    = Ok Flashcard
    | Err String


type Model
    = AddFlashcard Flashcard (List Flashcard) (Maybe String)
    | NoFlashCardsRemain
    | ShowWord Flashcard (List Flashcard)
    | ShowDefinition Flashcard (List Flashcard)
    | Problem String


initialModel : Model
initialModel =
    ShowWord
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



---- UPDATE ----


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
                            ( ShowWord flashCard (currentFlashcardList ++ [ flashCard ]), Cmd.none )

                        Err errorMsg ->
                            ( AddFlashcard flashcardDetails flashcards (Just errorMsg), Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'adding a flashcard'. Please refresh the browser, if this persists, contact the adminstrator.", Cmd.none )

        ShowWord currentFlashcard flashcards ->
            case msg of
                ShowNext ->
                    ( ShowWord currentFlashcard flashcards
                    , Random.generate DisplayRandomFlashcard (shuffle flashcards)
                    )

                DisplayRandomFlashcard shuffledFlashcards ->
                    case List.head shuffledFlashcards of
                        Nothing ->
                            ( NoFlashCardsRemain, Cmd.none )

                        Just flashcard ->
                            ( ShowWord flashcard shuffledFlashcards, Cmd.none )

                DeleteFlashcard ->
                    let
                        filteredFlashcards =
                            List.filter (\f -> f /= currentFlashcard) flashcards
                    in
                    case List.head filteredFlashcards of
                        Just flashcard ->
                            ( ShowWord flashcard filteredFlashcards
                            , Random.generate DisplayRandomFlashcard (shuffle filteredFlashcards)
                            )

                        Nothing ->
                            ( NoFlashCardsRemain, Cmd.none )

                HandleAdd ->
                    ( AddFlashcard { word = Nothing, definition = Nothing } flashcards Nothing, Cmd.none )

                ToggleFlashcard ->
                    ( ShowDefinition currentFlashcard flashcards, Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'showing word'. Please refresh the browser, if this persists, contact the adminstrator.", Cmd.none )

        ShowDefinition currentFlashcard flashcards ->
            case msg of
                ShowNext ->
                    ( ShowDefinition currentFlashcard flashcards
                    , Random.generate DisplayRandomFlashcard (shuffle flashcards)
                    )

                DisplayRandomFlashcard shuffledFlashcards ->
                    case List.head shuffledFlashcards of
                        Nothing ->
                            ( NoFlashCardsRemain, Cmd.none )

                        Just flashcard ->
                            ( ShowDefinition flashcard shuffledFlashcards, Cmd.none )

                DeleteFlashcard ->
                    let
                        filteredFlashcards =
                            List.filter (\f -> f /= currentFlashcard) flashcards
                    in
                    case List.head filteredFlashcards of
                        Just flashcard ->
                            ( ShowDefinition flashcard filteredFlashcards
                            , Random.generate DisplayRandomFlashcard (shuffle filteredFlashcards)
                            )

                        Nothing ->
                            ( NoFlashCardsRemain, Cmd.none )

                HandleAdd ->
                    ( AddFlashcard { word = Nothing, definition = Nothing } flashcards Nothing, Cmd.none )

                ToggleFlashcard ->
                    ( ShowWord currentFlashcard flashcards, Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'showing definition'. Please refresh the browser, if this persists, contact the adminstrator.", Cmd.none )

        NoFlashCardsRemain ->
            case msg of
                HandleAdd ->
                    ( AddFlashcard { word = Nothing, definition = Nothing } [] Nothing, Cmd.none )

                _ ->
                    ( NoFlashCardsRemain, Cmd.none )

        Problem _ ->
            ( model, Cmd.none )



---- VIEW ----


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


showingFlashcard : Maybe String -> Element Msg
showingFlashcard wordOrDef =
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
            [ primaryButton "Next" <| Just ShowNext
            , primaryButton "Add" <| Just HandleAdd
            , dangerButton "Delete" <| Just DeleteFlashcard
            ]
        ]


view : Model -> Element Msg
view model =
    case model of
        AddFlashcard flashcard flashcards errorString ->
            addingFlashcardView flashcard flashcards errorString

        ShowWord { word } _ ->
            showingFlashcard word

        ShowDefinition { definition } _ ->
            showingFlashcard definition

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
