module ElementLibrary.Elements exposing
    ( Message
    , button
    , buttonImage
    , editingFlashcard
    , flashcard
    , heading
    , inputField
    , message
    , multilineInputField
    , passwordInputField
    , searchField
    , shuffleButton
    )

import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , paddingEach
        , paragraph
        , text
        , width
        )
import Element.Input as Input
import ElementLibrary.Helpers exposing (MessageType(..), edges)
import ElementLibrary.Style as Style



-- DISPLAY ONLY


type alias Message =
    { messageType : MessageType
    , messageString : String
    }


message : Message -> Element msg
message { messageType, messageString } =
    el
        (case messageType of
            Error ->
                Style.errorMessage

            Info ->
                Style.infoMessage

            Successful ->
                Style.successfulMessage
        )
    <|
        paragraph [ width fill ] [ text messageString ]



-- HEADINGS


heading : String -> Element msg
heading str =
    el Style.heading <|
        paragraph [] [ text str ]



-- INPUT FIELDS


type alias InputFieldValues msg =
    { fieldTitle : String
    , messageOnChange : String -> msg
    , fieldValue : String
    }


inputField : InputFieldValues msg -> Element msg
inputField { fieldTitle, messageOnChange, fieldValue } =
    Input.text Style.inputField
        { onChange = messageOnChange
        , text = fieldValue
        , placeholder = Nothing
        , label = Style.inputFieldLabel fieldTitle
        }


multilineInputField : InputFieldValues msg -> Element msg
multilineInputField { fieldTitle, messageOnChange, fieldValue } =
    Input.multiline Style.inputField
        { onChange = messageOnChange
        , text = fieldValue
        , placeholder = Nothing
        , label = Style.inputFieldLabel fieldTitle
        , spellcheck = False
        }


type alias SearchFieldValues msg =
    { messageOnChange : String -> msg
    , fieldValue : String
    , onEnterMsg : msg
    }


searchField : SearchFieldValues msg -> Element msg
searchField { onEnterMsg, messageOnChange, fieldValue } =
    Input.text (Style.searchField onEnterMsg)
        { onChange = messageOnChange
        , text = fieldValue
        , placeholder = Just (Input.placeholder [] <| text "Search...")
        , label =
            Input.labelLeft [ paddingEach { edges | top = 10 } ] <|
                Element.image
                    (Style.image <| Just 20)
                    { src = "./images/icons/search.svg"
                    , description = "Search"
                    }
        }


passwordInputField : InputFieldValues msg -> Element msg
passwordInputField { fieldTitle, messageOnChange, fieldValue } =
    column
        [ width fill
        ]
        [ Input.currentPassword Style.inputField
            { onChange = messageOnChange
            , text = fieldValue
            , placeholder = Nothing
            , label = Style.inputFieldLabel fieldTitle
            , show = False
            }
        ]



-- BUTTONS


shuffleButton : Bool -> Maybe msg -> Element msg
shuffleButton shuffleOn onClickMsg =
    Input.button Style.buttonImage
        { onPress = onClickMsg
        , label =
            Element.image (Style.shuffleImage shuffleOn)
                { src = "./images/icons/shuffle.svg"
                , description = "Shuffle"
                }
        }


buttonImage : { onClickMsg : Maybe msg, src : String, description : String, specifiedImageHeight : Maybe Int } -> Element msg
buttonImage { onClickMsg, src, description, specifiedImageHeight } =
    Input.button Style.buttonImage
        { onPress = onClickMsg
        , label =
            Element.image (Style.image specifiedImageHeight)
                { src = src
                , description = description
                }
        }


button : String -> Maybe msg -> Element msg
button str onClickMsg =
    Input.button
        (case onClickMsg of
            Nothing ->
                Style.button True

            Just _ ->
                Style.button False
        )
        { onPress = onClickMsg
        , label = text str
        }


flashcard : { onClickMsg : msg, isEditable : Bool, label : Element msg } -> Element msg
flashcard { onClickMsg, isEditable, label } =
    Input.button
        (Style.flashcard isEditable)
        { onPress = Just onClickMsg
        , label =
            if isEditable then
                column [ width fill ]
                    [ el
                        [ Element.alignTop
                        , Element.alignRight
                        ]
                      <|
                        Element.image (Style.image <| Just 20)
                            { src = "./images/icons/pencil.svg"
                            , description = "Edit"
                            }
                    , label
                    ]

            else
                label
        }


editingFlashcard : { onClickMsg : msg, onUpdateWordMsg : String -> msg, onUpdateDefinitionMsg : String -> msg, word : String, definition : String } -> Element msg
editingFlashcard { onClickMsg, onUpdateWordMsg, onUpdateDefinitionMsg, word, definition } =
    flashcard
        { isEditable = True
        , onClickMsg = onClickMsg
        , label =
            Element.row []
                [ Input.text Style.flashcardNoPadding
                    { onChange = onUpdateWordMsg
                    , text = word
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft Style.flashcardNoPadding <| Element.text "Word: "
                    }
                , Input.multiline Style.flashcardNoPadding
                    { onChange = onUpdateDefinitionMsg
                    , text = definition
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft Style.flashcardNoPadding <| Element.text "Definition: "
                    , spellcheck = False
                    }
                ]
        }
