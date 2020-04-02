module ElementLibrary.Elements exposing (dangerButton, primaryButton, successButton)

import Element exposing (Element, text)
import Element.Input as Input
import ElementLibrary.Style as Style exposing (ButtonType(..))


primaryButton : Maybe msg -> String -> Element msg
primaryButton onClickMsg str =
    button onClickMsg Primary str


dangerButton : Maybe msg -> String -> Element msg
dangerButton onClickMsg str =
    button onClickMsg Danger str


successButton : Maybe msg -> String -> Element msg
successButton onClickMsg str =
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
