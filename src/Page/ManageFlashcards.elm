module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Api exposing (httpError, saveFlashcardRequest)
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
    | NoFlashcards
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


initialModel : String -> Model
initialModel idToken =
    { idToken = idToken
    , state = NoFlashcards
    }



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
    | EditFlashcard


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

                _ ->
                    ( state, Cmd.none )

        NoFlashcards ->
            case msg of
                Add ->
                    ( AddingAFlashcard Nothing { word = "", definition = "" } Nothing, Cmd.none )

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
                            ( NoFlashcards, Cmd.none )

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


noFlashcardsDisplay : Element Msg
noFlashcardsDisplay =
    column
        [ width fill
        , spacing 20
        , padding 20
        , centerX
        ]
        [ heading "No Flash cards exist - add one"
        , el [ centerX ] addFlashcardButton
        ]


view : Model -> Element Msg
view { state } =
    case state of
        FlashcardsExist { searchString, flashcards } ->
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
                    (flashcards
                        |> Dict.map
                            (\_ { word, definition } ->
                                ElementLibrary.Elements.flashcard
                                    { isEditable = True
                                    , onClickMsg = EditFlashcard
                                    , label =
                                        paragraph []
                                            [ text <| "Word: " ++ word ++ ", "
                                            , text <| "Definition: " ++ definition
                                            ]
                                    }
                            )
                        |> Dict.values
                    )
                ]

        NoFlashcards ->
            noFlashcardsDisplay

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
