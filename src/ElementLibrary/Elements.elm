module ElementLibrary.Elements exposing (checkbox, dangerButton, errorMessage, infoMessage, inputField, primaryButton, successButton, successfulMessage)

import Element exposing (Element, column, el, fill, paragraph, text, width)
import Element.Input as Input
import ElementLibrary.Style as Style exposing (ButtonType(..), MessageType(..))



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



-- INPUT FIELDS


type alias InputFieldValues msg =
    { fieldTitle : String
    , messageOnChange : String -> msg
    , fieldValue : String
    }


inputField : InputFieldValues msg -> Element msg
inputField { fieldTitle, messageOnChange, fieldValue } =
    column
        [ width fill
        ]
        [ Input.text Style.inputField
            { onChange = messageOnChange
            , text = fieldValue
            , placeholder = Nothing
            , label = Input.labelAbove [] (text fieldTitle)
            }
        ]


checkbox :
    { messageOnChecked : Bool -> msg
    , labelText : String
    , boxChecked : Bool
    }
    -> Element msg
checkbox { messageOnChecked, labelText, boxChecked } =
    column [ width fill ]
        [ Input.checkbox []
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
