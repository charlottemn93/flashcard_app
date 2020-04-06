module ElementLibrary.Elements exposing (button, errorMessage, flashcard, heading, infoMessage, inputField, nextButton, passwordInputField, shuffleButton, successfulMessage)

import Element exposing (Element, column, el, fill, paddingEach, paragraph, text, width)
import Element.Input as Input
import ElementLibrary.Style as Style exposing (MessageType(..), edges)



-- DISPLAY ONLY


errorMessage : String -> Element msg
errorMessage messageStr =
    message Error messageStr


infoMessage : String -> Element msg
infoMessage messageStr =
    message Info messageStr


successfulMessage : String -> Element msg
successfulMessage messageStr =
    message Successful messageStr


message : MessageType -> String -> Element msg
message messageType messageStr =
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
        paragraph [ width fill ] [ text messageStr ]



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
    , required : Bool
    }


inputField : InputFieldValues msg -> Element msg
inputField { fieldTitle, messageOnChange, fieldValue, required } =
    column
        [ width fill
        ]
        [ Input.text Style.inputField
            { onChange = messageOnChange
            , text = fieldValue
            , placeholder = Nothing
            , label = Style.inputFieldLabel fieldTitle
            }
        , if required == True && String.isEmpty fieldValue then
            el
                (Style.dangerText
                    ++ [ paddingEach { edges | top = 5, bottom = 20 } ]
                )
            <|
                text <|
                    fieldTitle
                        ++ " is required"

          else
            Element.none
        ]


passwordInputField : InputFieldValues msg -> Element msg
passwordInputField { fieldTitle, messageOnChange, fieldValue, required } =
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
        , if required == True && String.isEmpty fieldValue then
            el
                (Style.dangerText
                    ++ [ paddingEach { edges | top = 5, bottom = 20 } ]
                )
            <|
                text <|
                    fieldTitle
                        ++ " is required"

          else
            Element.none
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


nextButton : Maybe msg -> Element msg
nextButton onClickMsg =
    Input.button Style.buttonImage
        { onPress = onClickMsg
        , label =
            Element.image Style.image
                { src = "./images/icons/right-arrow.svg"
                , description = "Previous"
                }
        }


previousButton : Maybe msg -> Element msg
previousButton onClickMsg =
    Input.button []
        { onPress = onClickMsg
        , label =
            Element.image []
                { src = "./images/icons/left-arrow.svg"
                , description = "Next"
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


flashcard : msg -> String -> Element msg
flashcard onClickMsg str =
    Input.button
        Style.flashcard
        { onPress = Just onClickMsg
        , label = paragraph [] [ text str ]
        }
