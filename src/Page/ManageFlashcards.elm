module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, paddingXY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import ElementLibrary.Elements exposing (buttonImage, heading, paginatedContainer, searchField)
import Flashcard exposing (Flashcard)



-- MODEL


type Model
    = FlashcardsExist
        { searchString : String
        , flashcards : Dict Int Flashcard
        , pageNumber : Int
        }
    | NoFlashcards
    | AddingAFlashcard
        { word : String
        , definition : String
        }


initialModel : Model
initialModel =
    NoFlashcards



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        FlashcardsExist details ->
            case msg of
                UpdateSearchField str ->
                    ( FlashcardsExist { details | searchString = str }, Cmd.none )

                Search ->
                    ( FlashcardsExist { details | searchString = "Search activated!" }, Cmd.none )

                ChangePage pageNumber ->
                    ( FlashcardsExist { details | pageNumber = pageNumber }, Cmd.none )

                Add ->
                    ( AddingAFlashcard { word = "", definition = "" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoFlashcards ->
            case msg of
                Add ->
                    ( AddingAFlashcard { word = "", definition = "" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddingAFlashcard details ->
            case msg of
                Save ->
                    -- todo: http request
                    ( model, Cmd.none )

                CancelAdd ->
                    -- todo: go back to the state before - actually need this to act like an overlay
                    ( model, Cmd.none )

                UpdateWord word ->
                    ( AddingAFlashcard { details | word = word }, Cmd.none )

                UpdateDefinition definition ->
                    ( AddingAFlashcard { details | definition = definition }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



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
view model =
    case model of
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
                , paginatedContainer
                    { items =
                        flashcards
                            |> Dict.values
                            |> List.map (\card -> flashcardRow card)
                    , numberOfItemsPerPage = numberOfItemsPerPage
                    , currentPage = pageNumber
                    , changePageMessage = ChangePage
                    }
                ]

        NoFlashcards ->
            noFlashcardsDisplay

        AddingAFlashcard { word, definition } ->
            addingAFlashcardView
                { word = word
                , definition = definition
                }



-- todo: add this is the element library (overlay?)


addingAFlashcardView : { word : String, definition : String } -> Element Msg
addingAFlashcardView { word, definition } =
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
            [ text "Add a flashcard"
            , el [ Element.alignRight ]
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
