module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Api exposing (httpError, saveFlashcardRequest)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, paddingXY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
        , pageNumber : Int
        }
    | NoFlashcards
    | AddingAFlashcard
        (Maybe
            { searchString : String
            , flashcards : Dict Int Flashcard
            , pageNumber : Int
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
    | ChangePage Int
    | Add
    | Save
    | UpdateWord String
    | UpdateDefinition String
    | CancelAdd
    | FlashcardSaved (Result Http.Error Flashcard)
    | GotTimeNow Posix


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

                ChangePage pageNumber ->
                    ( FlashcardsExist { details | pageNumber = pageNumber }, Cmd.none )

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
                                    , pageNumber = 1
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


numberOfItemsPerPage : Int
numberOfItemsPerPage =
    10


flashcardRow : Flashcard -> Element msg
flashcardRow { word, definition, createdDateTime } =
    --todo
    text "Flashcard row"


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
        FlashcardsExist { searchString, flashcards, pageNumber } ->
            column
                [ width fill
                , spacing 20
                , padding 20
                ]
                [ el [ width fill, paddingXY 50 100 ] <|
                    searchField
                        { messageOnChange = UpdateSearchField
                        , fieldValue = searchString
                        , onEnterMsg = Search
                        }
                , addFlashcardButton

                -- , flashcards
                ]

        NoFlashcards ->
            noFlashcardsDisplay

        AddingAFlashcard _ { word, definition } message ->
            addingAFlashcardView
                { word = word
                , definition = definition
                }
                message



-- todo: add this is the element library (overlay?)


addingAFlashcardView : { word : String, definition : String } -> Maybe Message -> Element Msg
addingAFlashcardView { word, definition } message =
    column
        [ centerX
        , Element.centerY
        , padding 20
        , spacing 10
        , Background.color <| Element.rgb255 255 255 255
        , Element.moveLeft 60
        , Border.shadow
            { color = Element.rgba255 0 0 0 0.2
            , blur = 4
            , offset = ( 0, 0 )
            , size = 900
            }
        ]
        [ Element.row
            [ Font.bold
            , Font.size 16
            , Border.widthEach
                { bottom = 1
                , right = 0
                , top = 0
                , left = 0
                }
            , Border.solid
            , Border.color <| Element.rgb255 221 221 221
            , width fill
            , paddingXY 0 10
            ]
            [ case message of
                Just { messageType, messageString } ->
                    ElementLibrary.Elements.message
                        { messageType = messageType
                        , messageString = messageString
                        }

                Nothing ->
                    text "Add a flashcard"
            , el [ Element.alignRight, paddingXY 10 0 ]
                (buttonImage
                    { onClickMsg = Just CancelAdd
                    , src = "./images/icons/cancel.svg"
                    , description = "Cancel add"
                    , specifiedImageHeight = Just 15
                    }
                )
            ]
        , Element.row
            [ Element.paddingXY 30 0
            , spacing 20
            ]
            [ ElementLibrary.Elements.inputField
                { fieldTitle = "Word"
                , messageOnChange = UpdateWord
                , fieldValue = word
                }
            , ElementLibrary.Elements.inputField
                { fieldTitle = "Definition"
                , messageOnChange = UpdateDefinition
                , fieldValue = definition
                }
            ]
        , if String.isEmpty word || String.isEmpty definition then
            el [ padding 30 ] <| ElementLibrary.Elements.button "Save" Nothing

          else
            el [ padding 30 ] <| ElementLibrary.Elements.button "Save" <| Just Save
        ]
