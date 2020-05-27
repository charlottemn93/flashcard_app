module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Api exposing (deleteFlashcardRequest, editFlashcardRequest, httpError, loadFlashcardsRequest, saveFlashcardRequest)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, paddingXY, paragraph, spacing, text, width)
import ElementLibrary.Elements exposing (Message, buttonImage, heading)
import ElementLibrary.Helpers exposing (MessageType(..))
import Flashcard as Flashcard exposing (Flashcard)
import Http as Http
import Task
import Time as Time exposing (Posix)



-- MODEL


type alias Model =
    { idToken : String
    , state : State
    , searchString : String
    , flashcards : Dict Int Flashcard
    }


type State
    = Ready String
    | EditingFlashcard
        Int
        { word : String
        , definition : String
        , message : Maybe Message
        }
    | AddingAFlashcard
        { word : String
        , definition : String
        }
        (Maybe Message)


initialModel : String -> ( Model, Cmd Msg )
initialModel idToken =
    ( { idToken = idToken
      , state = Ready ""
      , searchString = ""
      , flashcards = Dict.empty
      }
    , loadFlashcardsRequest
        { idToken = idToken
        , expectMsg = LoadFlashcards
        }
    )



-- UPDATE


type Msg
    = Add
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


search : String -> Dict Int Flashcard -> Dict Int Flashcard
search searchString allFlashcards =
    allFlashcards
        |> Dict.filter
            (\_ { word, definition } ->
                let
                    lowercaseWord =
                        String.toLower word

                    lowercaseDef =
                        String.toLower definition

                    lowercaseSearchStr =
                        String.toLower searchString
                in
                String.startsWith lowercaseSearchStr lowercaseWord
                    || String.startsWith lowercaseSearchStr lowercaseDef
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Ready _ ->
            case msg of
                Add ->
                    ( { model
                        | state = AddingAFlashcard { word = "", definition = "" } Nothing
                      }
                    , Cmd.none
                    )

                EditFlashcard index ->
                    case
                        Flashcard.fromIndex index model.flashcards
                    of
                        Nothing ->
                            -- should not happen
                            ( model, Cmd.none )

                        Just { word, definition } ->
                            ( { model
                                | state =
                                    EditingFlashcard index
                                        { word = word
                                        , definition = definition
                                        , message = Nothing
                                        }
                              }
                            , Cmd.none
                            )

                LoadFlashcards (Ok cards) ->
                    ( { model
                        | state = Ready ""
                        , flashcards = Flashcard.fromList cards
                      }
                    , Cmd.none
                    )

                LoadFlashcards (Err e) ->
                    ( { model
                        | state = Ready <| httpError e
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditingFlashcard index details ->
            case msg of
                UpdateWord word ->
                    ( { model
                        | state = EditingFlashcard index { details | word = word }
                      }
                    , Cmd.none
                    )

                UpdateDefinition definition ->
                    ( { model
                        | state = EditingFlashcard index { details | definition = definition }
                      }
                    , Cmd.none
                    )

                Save ->
                    ( model, Task.perform GotTimeNow Time.now )

                GotTimeNow posix ->
                    case Flashcard.fromIndex index model.flashcards of
                        Just { id } ->
                            ( model
                            , editFlashcardRequest
                                { definition = details.definition
                                , word = details.word
                                , id = id
                                , idToken = model.idToken
                                , posix = posix
                                , expectMsg = FlashcardSaved
                                }
                            )

                        Nothing ->
                            ( model
                            , saveFlashcardRequest
                                { definition = details.definition
                                , word = details.word
                                , idToken = model.idToken
                                , posix = posix
                                , expectMsg = FlashcardSaved
                                }
                            )

                Delete ->
                    case Flashcard.fromIndex index model.flashcards of
                        Just { id } ->
                            ( model
                            , deleteFlashcardRequest
                                { id = id
                                , idToken = model.idToken
                                , expectMsg = FlashcardDeleted
                                }
                            )

                        Nothing ->
                            ( { model
                                | state =
                                    EditingFlashcard index
                                        { details
                                            | message = Just { messageType = Error, messageString = "Cannot delete flashcard that doesn't exist" }
                                        }
                              }
                            , Cmd.none
                            )

                FlashcardSaved (Err e) ->
                    ( { model
                        | state = EditingFlashcard index { details | message = Just { messageType = Error, messageString = httpError e } }
                      }
                    , Cmd.none
                    )

                FlashcardSaved (Ok _) ->
                    ( { model
                        | state =
                            EditingFlashcard index
                                { details
                                    | message = Just { messageType = Successful, messageString = "Flashcard updated" }
                                }
                      }
                    , Cmd.none
                    )

                GoBack ->
                    ( { model
                        | state = Ready ""
                      }
                    , loadFlashcardsRequest
                        { idToken = model.idToken
                        , expectMsg = LoadFlashcards
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        AddingAFlashcard details message ->
            case msg of
                Save ->
                    ( model, Task.perform GotTimeNow Time.now )

                GotTimeNow posix ->
                    ( model
                    , saveFlashcardRequest
                        { posix = posix
                        , word = details.word
                        , definition = details.definition
                        , idToken = model.idToken
                        , expectMsg = FlashcardSaved
                        }
                    )

                FlashcardSaved (Err e) ->
                    ( { model
                        | state = AddingAFlashcard details <| Just { messageType = Error, messageString = httpError e }
                      }
                    , Cmd.none
                    )

                FlashcardSaved (Ok _) ->
                    ( { model
                        | state =
                            AddingAFlashcard
                                { details | word = "", definition = "" }
                            <|
                                Just { messageType = Successful, messageString = "Flashcard saved" }
                      }
                    , Cmd.none
                    )

                GoBack ->
                    ( { model
                        | state = Ready ""
                      }
                    , Cmd.none
                    )

                UpdateWord word ->
                    ( { model
                        | state = AddingAFlashcard { details | word = word } message
                      }
                    , Cmd.none
                    )

                UpdateDefinition definition ->
                    ( { model
                        | state = AddingAFlashcard { details | definition = definition } message
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



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

        -- todo: ElementLibrary.Elements.searchField
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
view model =
    case model.state of
        Ready errorString ->
            if Dict.isEmpty model.flashcards then
                noFlashcardsDisplay errorString

            else
                manageFlashcardsView model.searchString <| defaultFlashcardsContent model.flashcards

        EditingFlashcard index { word, definition, message } ->
            case Flashcard.fromIndex index model.flashcards of
                Nothing ->
                    manageFlashcardsView model.searchString <| defaultFlashcardsContent model.flashcards

                Just _ ->
                    editFlashcardView
                        { word = word
                        , definition = definition
                        }
                        message

        AddingAFlashcard { word, definition } message ->
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
