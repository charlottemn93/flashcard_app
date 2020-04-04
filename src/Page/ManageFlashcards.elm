module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element)
import Html exposing (Html, br, button, div, h1, h2, img, input, label, node, span, text)
import Html.Attributes exposing (disabled, id, src)
import Html.Events exposing (onClick, onInput)
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
    = AddFlashcard Flashcard (List Flashcard)
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
      -- DisplayRandomFlashcard ?
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
    -- Model
    -- = AddFlashcard Flashcard (List Flashcard)
    -- | NoFlashCardsRemain
    -- | ShowWord Flashcard (List Flashcard)
    -- | ShowDefinition Flashcard (List Flashcard)
    -- | Problem String
    -- type Msg
    --     = ShowNext
    --     | DisplayRandomFlashcard (List Flashcard)
    --     | ToggleFlashcard
    --     | DeleteFlashcard
    --     | HandleAdd
    --     | UpdateWord String
    --     | UpdateDefinition String
    --     | SaveFlashcard Flashcard (List Flashcard)
    case model of
        -- todo: why update word in the middle of adding a flashcard? Take a look at this
        AddFlashcard flashcardDetails flashcards ->
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
                            ( Problem errorMsg, Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'adding a flashcard'", Cmd.none )

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
                    ( AddFlashcard { word = Nothing, definition = Nothing } flashcards, Cmd.none )

                ToggleFlashcard ->
                    ( ShowDefinition currentFlashcard flashcards, Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'showing word'", Cmd.none )

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
                    ( AddFlashcard { word = Nothing, definition = Nothing } flashcards, Cmd.none )

                ToggleFlashcard ->
                    ( ShowWord currentFlashcard flashcards, Cmd.none )

                _ ->
                    ( Problem "Received message in unexpected state, state is 'showing definition'", Cmd.none )

        NoFlashCardsRemain ->
            case msg of
                HandleAdd ->
                    ( AddFlashcard { word = Nothing, definition = Nothing } [], Cmd.none )

                _ ->
                    ( NoFlashCardsRemain, Cmd.none )

        Problem _ ->
            -- todo: add "go back" functionality
            ( model, Cmd.none )



---- VIEW ----
-- todo: change to elm UI


addingFlashcardView : Flashcard -> List Flashcard -> Html Msg
addingFlashcardView { word, definition } flashcards =
    div []
        [ h1 [] [ text "Flash 'em - the flash card app" ]
        , label [] [ text "Word" ]
        , input [ onInput UpdateWord ] []
        , br [] []
        , label [] [ text "Definition" ]
        , input [ onInput UpdateDefinition ] []
        , br [] []
        , button
            [ onClick (SaveFlashcard { word = word, definition = definition } flashcards) ]
            [ text "Save" ]
        ]



-- todo: change to elm UI


showingFlashcardDefinition : Flashcard -> Html Msg
showingFlashcardDefinition { definition } =
    div []
        [ h1 [] [ text "Flash 'em - the flash card app" ]
        , node "flashcard"
            [ onClick ToggleFlashcard ]
            [ text <|
                case definition of
                    Nothing ->
                        ""

                    Just def ->
                        def
            ]
        , br [] []
        , button
            [ onClick ShowNext ]
            [ text "Next" ]
        , br [] []
        , button
            [ onClick HandleAdd ]
            [ text "Add" ]
        , button
            [ onClick DeleteFlashcard ]
            [ text "Delete" ]
        ]


showingFlashcardWord : Flashcard -> Html Msg
showingFlashcardWord { word } =
    div []
        [ h1 [] [ text "Flash 'em - the flash card app" ]
        , node "flashcard"
            [ onClick ToggleFlashcard ]
            [ text <|
                case word of
                    Nothing ->
                        ""

                    Just str ->
                        str
            ]
        , br [] []
        , button
            [ onClick ShowNext ]
            [ text "Next" ]
        , br [] []
        , button
            [ onClick HandleAdd ]
            [ text "Add" ]
        , button
            [ onClick DeleteFlashcard ]
            [ text "Delete" ]
        ]



-- todo: change to elm UI


view : Model -> Element Msg
view model =
    Element.html <|
        case model of
            AddFlashcard flashcard flashcards ->
                addingFlashcardView flashcard flashcards

            ShowWord currentFlashcard _ ->
                showingFlashcardWord currentFlashcard

            ShowDefinition currentFlashcard _ ->
                showingFlashcardDefinition currentFlashcard

            NoFlashCardsRemain ->
                div []
                    [ h2 [] [ text "No Flash Cards Remain - Add one!" ]
                    , br [] []
                    , button
                        [ onClick HandleAdd ]
                        [ text "Add" ]
                    ]

            Problem errorMessage ->
                div []
                    [ node "error"
                        []
                        [ text errorMessage
                        ]
                    , br [] []

                    -- todo: back button
                    ]
