module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Api exposing (editFlashcardRequest, httpError, loadFlashcardsRequest, saveFlashcardRequest)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, paddingXY, paragraph, spacing, text, width)
import ElementLibrary.Elements exposing (Message, buttonImage, heading, searchField)
import ElementLibrary.Helpers exposing (MessageType(..))
import Flashcard as Flashcard exposing (Flashcard)
import Http as Http
import Task
import Time as Time exposing (Posix)



-- MODEL


type alias Model =
    { idToken : String
    , state : State
    }


type State
    = FlashcardsExist
        { searchString : String
        , flashcards : Dict Int Flashcard
        }
    | EditingFlashcard
        Int
        { searchString : String
        , flashcards : Dict Int Flashcard
        , word : String
        , definition : String
        }
    | NoFlashcards String
    | AddingAFlashcard
        (Maybe
            { searchString : String
            , flashcards : Dict Int Flashcard
            }
        )
        { word : String
        , definition : String
        }
        (Maybe Message)


initialModel : String -> ( Model, Cmd Msg )
initialModel idToken =
    ( { idToken = idToken
      , state = NoFlashcards ""
      }
    , loadFlashcardsRequest
        { idToken = idToken
        , expectMsg = LoadFlashcards
        }
    )



-- UPDATE


type Msg
    = UpdateSearchField String
    | Search
    | Add
    | Save
    | UpdateWord String
    | UpdateDefinition String
    | CancelAdd
    | FlashcardSaved (Result Http.Error Flashcard)
    | GotTimeNow Posix
    | EditFlashcard Int
    | SaveEdits
    | LoadFlashcards (Result Http.Error (List Flashcard))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( state, command ) =
            updateState msg model
    in
    ( { model | state = state }, command )


updateState : Msg -> Model -> ( State, Cmd Msg )
updateState msg { state, idToken } =
    case state of
        FlashcardsExist details ->
            case msg of
                UpdateSearchField str ->
                    ( FlashcardsExist { details | searchString = str }, Cmd.none )

                Search ->
                    ( FlashcardsExist { details | searchString = "Search activated!" }, Cmd.none )

                Add ->
                    ( AddingAFlashcard (Just details) { word = "", definition = "" } Nothing, Cmd.none )

                EditFlashcard index ->
                    case
                        Flashcard.fromIndex index details.flashcards
                    of
                        Nothing ->
                            -- should not happen
                            ( state, Cmd.none )

                        Just { word, definition } ->
                            ( EditingFlashcard index
                                { flashcards = details.flashcards
                                , searchString = details.searchString
                                , word = word
                                , definition = definition
                                }
                            , Cmd.none
                            )

                _ ->
                    ( state, Cmd.none )

        EditingFlashcard index details ->
            case msg of
                UpdateWord word ->
                    ( EditingFlashcard index { details | word = word }, Cmd.none )

                UpdateDefinition definition ->
                    ( EditingFlashcard index { details | definition = definition }, Cmd.none )

                SaveEdits ->
                    ( state, Task.perform GotTimeNow Time.now )

                GotTimeNow posix ->
                    case Flashcard.fromIndex index details.flashcards of
                        Just { id } ->
                            ( state
                            , editFlashcardRequest
                                { definition = details.definition
                                , word = details.word
                                , id = id
                                , idToken = idToken
                                , posix = posix
                                , expectMsg = FlashcardSaved
                                }
                            )

                        Nothing ->
                            ( state
                            , saveFlashcardRequest
                                { definition = details.definition
                                , word = details.word
                                , idToken = idToken
                                , posix = posix
                                , expectMsg = FlashcardSaved
                                }
                            )

                _ ->
                    ( state, Cmd.none )

        NoFlashcards _ ->
            case msg of
                Add ->
                    ( AddingAFlashcard Nothing { word = "", definition = "" } Nothing, Cmd.none )

                LoadFlashcards (Ok flashcards) ->
                    ( FlashcardsExist
                        { searchString = ""
                        , flashcards = Flashcard.fromList flashcards
                        }
                    , Cmd.none
                    )

                LoadFlashcards (Err e) ->
                    ( NoFlashcards <| httpError e, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        AddingAFlashcard searchDetails details message ->
            case msg of
                Save ->
                    ( state, Task.perform GotTimeNow Time.now )

                GotTimeNow posix ->
                    ( state
                    , saveFlashcardRequest
                        { posix = posix
                        , word = details.word
                        , definition = details.definition
                        , idToken = idToken
                        , expectMsg = FlashcardSaved
                        }
                    )

                FlashcardSaved (Err e) ->
                    ( AddingAFlashcard searchDetails details <| Just { messageType = Error, messageString = httpError e }, Cmd.none )

                FlashcardSaved (Ok newFlashcard) ->
                    let
                        updatedSearchDetails =
                            case searchDetails of
                                Just x ->
                                    { x
                                        | flashcards = Flashcard.add (Just x.flashcards) newFlashcard
                                    }

                                Nothing ->
                                    { flashcards = Flashcard.add Nothing newFlashcard
                                    , searchString = ""
                                    }
                    in
                    ( AddingAFlashcard
                        (Just updatedSearchDetails)
                        { details | word = "", definition = "" }
                      <|
                        Just { messageType = Successful, messageString = "Flashcard saved" }
                    , Cmd.none
                    )

                CancelAdd ->
                    case searchDetails of
                        Nothing ->
                            ( NoFlashcards "", Cmd.none )

                        Just x ->
                            ( FlashcardsExist x, Cmd.none )

                UpdateWord word ->
                    ( AddingAFlashcard searchDetails { details | word = word } message, Cmd.none )

                UpdateDefinition definition ->
                    ( AddingAFlashcard searchDetails { details | definition = definition } message, Cmd.none )

                _ ->
                    ( state, Cmd.none )



-- VIEW


addFlashcardButton : Element Msg
addFlashcardButton =
    el [ centerX ]
        (buttonImage
            { onClickMsg = Just Add
            , src = "./images/icons/add.svg"
            , description = "Add flashcard"
            , specifiedImageHeight = Nothing
            }
        )


noFlashcardsDisplay : String -> Element Msg
noFlashcardsDisplay errorString =
    column
        [ width fill
        , spacing 20
        , padding 20
        , centerX
        ]
        [ ElementLibrary.Elements.message
            { messageType = Error
            , messageString = errorString
            }
        , heading "No Flash cards exist - add one"
        , el [ centerX ] addFlashcardButton
        ]


manageFlashcardsView : String -> List (Element Msg) -> Element Msg
manageFlashcardsView searchString flashcardsContent =
    column
        [ width fill
        , spacing 20
        , padding 20
        ]
        [ el [ Element.alignTop, Element.alignLeft ] <| addFlashcardButton
        , el [ width fill, paddingXY 50 100 ] <|
            searchField
                { messageOnChange = UpdateSearchField
                , fieldValue = searchString
                , onEnterMsg = Search
                }
        , column
            [ width fill
            , spacing 10
            ]
            flashcardsContent
        ]


editingFlashcardsContent : { flashcards : Dict Int Flashcard, selectedIndex : Int, word : String, definition : String } -> List (Element Msg)
editingFlashcardsContent { flashcards, selectedIndex, word, definition } =
    flashcards
        |> Dict.map
            (\index f ->
                if index == selectedIndex then
                    ElementLibrary.Elements.editingFlashcard
                        { onClickMsg = SaveEdits
                        , onUpdateWordMsg = UpdateWord
                        , onUpdateDefinitionMsg = UpdateDefinition
                        , word = word
                        , definition = definition
                        }

                else
                    ElementLibrary.Elements.flashcard
                        { isEditable = True
                        , onClickMsg = EditFlashcard index
                        , label =
                            paragraph []
                                [ text <| "Word: " ++ f.word ++ ", "
                                , text <| "Definition: " ++ f.definition
                                ]
                        }
            )
        |> Dict.values


defaultFlashcardsContent : Dict Int Flashcard -> List (Element Msg)
defaultFlashcardsContent flashcards =
    flashcards
        |> Dict.map
            (\index { word, definition } ->
                ElementLibrary.Elements.flashcard
                    { isEditable = True
                    , onClickMsg = EditFlashcard index
                    , label =
                        paragraph []
                            [ text <| "Word: " ++ word ++ ", "
                            , text <| "Definition: " ++ definition
                            ]
                    }
            )
        |> Dict.values


view : Model -> Element Msg
view { state } =
    case state of
        EditingFlashcard index { word, definition, searchString, flashcards } ->
            case Flashcard.fromIndex index flashcards of
                Nothing ->
                    manageFlashcardsView searchString <| defaultFlashcardsContent flashcards

                Just _ ->
                    manageFlashcardsView searchString <|
                        editingFlashcardsContent
                            { flashcards = flashcards
                            , selectedIndex = index
                            , word = word
                            , definition = definition
                            }

        FlashcardsExist { searchString, flashcards } ->
            manageFlashcardsView searchString <| defaultFlashcardsContent flashcards

        NoFlashcards errorString ->
            noFlashcardsDisplay errorString

        AddingAFlashcard _ { word, definition } message ->
            addingAFlashcardView
                { word = word
                , definition = definition
                }
                message


addingAFlashcardView : { word : String, definition : String } -> Maybe Message -> Element Msg
addingAFlashcardView { word, definition } message =
    column
        [ width fill
        ]
        [ case message of
            Just { messageType, messageString } ->
                ElementLibrary.Elements.message
                    { messageType = messageType
                    , messageString = messageString
                    }

            Nothing ->
                Element.none
        , column
            [ width fill
            , spacing 20
            , padding 20
            ]
            [ Element.row [ width fill ]
                [ buttonImage
                    { onClickMsg = Just CancelAdd
                    , src = "./images/icons/left-arrow.svg"
                    , description = "Go Back"
                    , specifiedImageHeight = Nothing
                    }
                , el
                    [ width fill, centerX ]
                  <|
                    heading "Add a flashcard"
                ]
            , Element.row
                [ spacing 20
                , width fill
                ]
                [ ElementLibrary.Elements.inputField
                    { fieldTitle = "Word"
                    , messageOnChange = UpdateWord
                    , fieldValue = word
                    }
                , ElementLibrary.Elements.multilineInputField
                    { fieldTitle = "Definition"
                    , messageOnChange = UpdateDefinition
                    , fieldValue = definition
                    }
                ]
            , if String.isEmpty word || String.isEmpty definition then
                ElementLibrary.Elements.button "Save" Nothing

              else
                ElementLibrary.Elements.button "Save" <| Just Save
            ]
        ]
