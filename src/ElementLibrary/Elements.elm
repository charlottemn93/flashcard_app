module ElementLibrary.Elements exposing (checkbox, dangerButton, errorMessage, heading, infoMessage, inputField, passwordInputField, primaryButton, successButton, successfulMessage)

import Element exposing (Element, column, el, fill, paddingEach, paragraph, text, width)
import Element.Input as Input
import ElementLibrary.Style as Style exposing (ButtonType(..), HeadingLevel(..), MessageType(..), edges)



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


heading : HeadingLevel -> String -> Element msg
heading headingLevel str =
    el
        (case headingLevel of
            One ->
                Style.heading1

            Two ->
                Style.heading2

            Three ->
                Style.heading3

            Four ->
                Style.heading4
        )
    <|
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


checkbox :
    { messageOnChecked : Bool -> msg
    , labelText : String
    , boxChecked : Bool
    }
    -> Element msg
checkbox { messageOnChecked, labelText, boxChecked } =
    column [ width fill ]
        [ Input.checkbox Style.checkbox
            { onChange = messageOnChecked
            , icon = Input.defaultCheckbox
            , checked = boxChecked
            , label = Input.labelRight [] (text labelText)
            }
        ]



-- BUTTONS


primaryButton : String -> Maybe msg -> Element msg
primaryButton str onClickMsg =
    button onClickMsg Primary str


dangerButton : String -> Maybe msg -> Element msg
dangerButton str onClickMsg =
    button onClickMsg Danger str


successButton : String -> Maybe msg -> Element msg
successButton str onClickMsg =
    button onClickMsg Success str


button : Maybe msg -> ButtonType -> String -> Element msg
button onClickMsg buttonType str =
    Input.button
        (case onClickMsg of
            Nothing ->
                Style.button
                    { buttonType = buttonType
                    , disabled = True
                    }

            Just _ ->
                Style.button
                    { buttonType = buttonType
                    , disabled = False
                    }
        )
        { onPress = onClickMsg
        , label = text str
        }
