module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Api exposing (deleteFlashcardRequest, editFlashcardRequest, httpError, loadFlashcardsRequest, saveFlashcardRequest)
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
        , message : Maybe Message
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
    | Delete
    | UpdateWord String
    | UpdateDefinition String
    | GoBack
    | FlashcardSaved (Result Http.Error Flashcard)
    | GotTimeNow Posix
    | EditFlashcard Int
    | LoadFlashcards (Result Http.Error (List Flashcard))
    | FlashcardDeleted (Result Http.Error String)


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
                                , message = Nothing
                                }
                            , Cmd.none
                            )

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

        EditingFlashcard index details ->
            case msg of
                UpdateWord word ->
                    ( EditingFlashcard index { details | word = word }, Cmd.none )

                UpdateDefinition definition ->
                    ( EditingFlashcard index { details | definition = definition }, Cmd.none )

                Save ->
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

                Delete ->
                    case Flashcard.fromIndex index details.flashcards of
                        Just { id } ->
                            ( state
                            , deleteFlashcardRequest
                                { id = id
                                , idToken = idToken
                                , expectMsg = FlashcardDeleted
                                }
                            )

                        Nothing ->
                            ( EditingFlashcard index { details | message = Just { messageType = Error, messageString = "Cannot delete flashcard that doesn't exist" } }
                            , Cmd.none
                            )

                FlashcardSaved (Err e) ->
                    ( EditingFlashcard index { details | message = Just { messageType = Error, messageString = httpError e } }, Cmd.none )

                FlashcardSaved (Ok _) ->
                    ( EditingFlashcard index { details | message = Just { messageType = Successful, messageString = "Flashcard updated" } }
                    , Cmd.none
                    )

                GoBack ->
                    ( FlashcardsExist
                        { searchString = details.searchString
                        , flashcards = details.flashcards
                        }
                    , loadFlashcardsRequest
                        { idToken = idToken
                        , expectMsg = LoadFlashcards
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

                FlashcardSaved (Ok _) ->
                    ( AddingAFlashcard
                        searchDetails
                        { details | word = "", definition = "" }
                      <|
                        Just { messageType = Successful, messageString = "Flashcard saved" }
                    , Cmd.none
                    )

                GoBack ->
                    case searchDetails of
                        Nothing ->
                            ( NoFlashcards ""
                            , loadFlashcardsRequest
                                { idToken = idToken
                                , expectMsg = LoadFlashcards
                                }
                            )

                        Just x ->
                            ( FlashcardsExist x
                            , loadFlashcardsRequest
                                { idToken = idToken
                                , expectMsg = LoadFlashcards
                                }
                            )

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
            , specifiedImageHeight = Just 56
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


manageFlashcardsView : String -> Element Msg -> Element Msg
manageFlashcardsView searchString flashcardsContent =
    column
        [ width fill
        , spacing 20
        , padding 50
        ]
        [ el [ Element.alignTop, Element.alignLeft, paddingXY 50 0 ] <| addFlashcardButton
        , el [ width fill, padding 50 ] <|
            searchField
                { messageOnChange = UpdateSearchField
                , fieldValue = searchString
                , onEnterMsg = Search
                }
        , flashcardsContent
        ]


defaultFlashcardsContent : Dict Int Flashcard -> Element Msg
defaultFlashcardsContent flashcards =
    let
        ( firstColumn, secondColumn ) =
            flashcards
                |> Dict.partition
                    (\index _ -> index < (Dict.size flashcards // 2))
    in
    Element.row
        [ width fill
        , spacing 20
        , paddingXY 50 0
        ]
        [ column
            [ width
                (fill
                    |> Element.maximum 500
                )
            , Element.height
                (fill
                    |> Element.maximum 500
                )
            , spacing 20
            ]
            (firstColumn
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
            )
        , column
            [ width
                (fill
                    |> Element.maximum 500
                )
            , Element.height
                (fill
                    |> Element.maximum 500
                )
            , spacing 20
            ]
            (secondColumn
                |> Dict.map
                    (\index { word, definition } ->
                        ElementLibrary.Elements.flashcard
                            { isEditable = True
                            , onClickMsg = EditFlashcard index
                            , label =
                                paragraph []
                                    [ text <| "Word: " ++ word ++ ", Definition: " ++ definition
                                    ]
                            }
                    )
                |> Dict.values
            )
        ]


view : Model -> Element Msg
view { state } =
    case state of
        EditingFlashcard index { word, definition, searchString, flashcards, message } ->
            case Flashcard.fromIndex index flashcards of
                Nothing ->
                    manageFlashcardsView searchString <| defaultFlashcardsContent flashcards

                Just _ ->
                    editFlashcardView
                        { word = word
                        , definition = definition
                        }
                        message

        FlashcardsExist { searchString, flashcards } ->
            manageFlashcardsView searchString <| defaultFlashcardsContent flashcards

        NoFlashcards errorString ->
            noFlashcardsDisplay errorString

        AddingAFlashcard _ { word, definition } message ->
            addFlashcardView
                { word = word
                , definition = definition
                }
                message


editFlashcardView : { word : String, definition : String } -> Maybe Message -> Element Msg
editFlashcardView { word, definition } message =
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
                    { onClickMsg = Just GoBack
                    , src = "./images/icons/left-arrow.svg"
                    , description = "Go Back"
                    , specifiedImageHeight = Nothing
                    }
                , el
                    [ width fill, centerX ]
                  <|
                    heading "Edit flashcard"
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
            , Element.row
                [ spacing 20
                , width fill
                ]
                [ if String.isEmpty word || String.isEmpty definition then
                    ElementLibrary.Elements.button "Save" Nothing

                  else
                    ElementLibrary.Elements.button "Save" <| Just Save
                , ElementLibrary.Elements.dangerButton "Delete" <| Just Delete
                ]
            ]
        ]


addFlashcardView : { word : String, definition : String } -> Maybe Message -> Element Msg
addFlashcardView { word, definition } message =
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
                    { onClickMsg = Just GoBack
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
