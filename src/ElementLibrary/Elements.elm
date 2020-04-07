module ElementLibrary.Elements exposing
    ( button
    , buttonImage
    , errorMessage
    , flashcard
    , heading
    , infoMessage
    , infoRow
    , inputField
    , paginatedContainer
    , passwordInputField
    , searchField
    , shuffleButton
    , successfulMessage
    )

import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , mouseOver
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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



-- INFO ROWS


type alias InfoItem msg =
    { title : String
    , content : Element msg
    }


infoItem : InfoItem msg -> Element msg
infoItem { title, content } =
    column Style.infoItemColumn
        [ paragraph
            []
            [ content
            ]
        , paragraph
            Style.infoItemLabel
            [ text title ]
        ]


infoRow : { contents : List (InfoItem msg), displayBorder : Bool } -> Element msg
infoRow { contents, displayBorder } =
    row
        (Style.infoRow displayBorder)
    <|
        List.map infoItem contents


infoColumn :
    { rows :
        List
            { contents : List (InfoItem msg)
            , heading : Maybe (Element msg)
            }
    , minimumRowLength : Maybe Int
    , displayBorderTop : Bool
    , displayBorderBottom : Bool
    }
    -> Element msg
infoColumn { rows, minimumRowLength, displayBorderTop, displayBorderBottom } =
    let
        minimumItemsToDisplay =
            Maybe.withDefault 0 minimumRowLength
    in
    column
        (Style.infoColumn
            { borderTop = displayBorderTop
            , borderBottom = displayBorderBottom
            }
        )
        (rows
            |> List.map
                (\row ->
                    column
                        [ width fill
                        , spacing 20
                        , paddingEach
                            { edges
                                | top = 20
                            }
                        ]
                        [ case row.heading of
                            Just element ->
                                element

                            Nothing ->
                                Element.none
                        , if List.length row.contents < minimumItemsToDisplay then
                            let
                                difference =
                                    minimumItemsToDisplay - List.length row.contents

                                filler =
                                    List.range 1 difference
                                        |> List.map (\_ -> { content = Element.none, title = "" })
                            in
                            infoRow
                                { contents = row.contents ++ filler
                                , displayBorder = False
                                }

                          else
                            infoRow
                                { contents = row.contents
                                , displayBorder = False
                                }
                        ]
                )
        )



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
    column
        [ width fill
        ]
        [ Input.text Style.inputField
            { onChange = messageOnChange
            , text = fieldValue
            , placeholder = Nothing
            , label = Style.inputFieldLabel fieldTitle
            }
        ]


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


flashcard : msg -> String -> Element msg
flashcard onClickMsg str =
    Input.button
        Style.flashcard
        { onPress = Just onClickMsg
        , label = paragraph [] [ text str ]
        }



-- PAGINATION


paginatedContainerButton :
    { onPress : Maybe msg
    , display : String
    , active : Bool
    , displayBorder : Bool
    }
    -> Element msg
paginatedContainerButton { onPress, display, active, displayBorder } =
    Input.button
        [ Border.solid
        , Border.color <| rgb255 221 221 221
        , if displayBorder then
            Border.widthEach { edges | right = 1 }

          else
            Border.width 0
        , Font.size 15
        , centerX
        ]
        { onPress = onPress
        , label =
            el
                (paddingXY 12 8
                    :: (case onPress of
                            Just _ ->
                                if active == False then
                                    [ Font.color Style.linkColour
                                    , mouseOver
                                        [ Background.color <| rgb255 238 238 238
                                        ]
                                    ]

                                else
                                    [ Font.color <| rgb255 255 255 255
                                    , Background.color <| Style.linkColour
                                    ]

                            Nothing ->
                                [ Font.color Style.mutedColour ]
                       )
                )
            <|
                text display
        }


paginatedContainer :
    { items : List (Element msg)
    , numberOfItemsPerPage : Int
    , currentPage : Int
    , changePageMessage : Int -> msg
    }
    -> Element msg
paginatedContainer { items, numberOfItemsPerPage, currentPage, changePageMessage } =
    let
        numberOfPages =
            ceiling <| toFloat (List.length items) / toFloat numberOfItemsPerPage

        lastPageNumberShown =
            (if remainderBy 5 currentPage == 0 then
                currentPage // 5

             else
                (currentPage // 5) + 1
            )
                * 5

        pageNumbersToDisplay =
            List.range (lastPageNumberShown - 4)
                (if lastPageNumberShown > numberOfPages then
                    numberOfPages

                 else
                    lastPageNumberShown
                )
    in
    column [ width fill ]
        ((items
            |> List.drop ((currentPage - 1) * numberOfItemsPerPage)
            |> List.take numberOfItemsPerPage
         )
            ++ [ el
                    [ width fill
                    , Region.navigation
                    , padding 20
                    ]
                 <|
                    row
                        [ centerX
                        , Border.rounded 3
                        , Border.width 1
                        , Border.color <| rgb255 221 221 221
                        ]
                        [ paginatedContainerButton
                            { onPress =
                                if currentPage == 1 then
                                    Nothing

                                else
                                    Just <| changePageMessage (currentPage - 1)
                            , display = "<"
                            , active = False
                            , displayBorder = True
                            }
                        , if currentPage > 5 then
                            paginatedContainerButton
                                { onPress =
                                    Just <|
                                        changePageMessage <|
                                            case List.head pageNumbersToDisplay of
                                                Just int ->
                                                    int - 1

                                                Nothing ->
                                                    1
                                , display = "..."
                                , active = False
                                , displayBorder = True
                                }

                          else
                            Element.none
                        , row []
                            (pageNumbersToDisplay
                                |> List.map
                                    (\x ->
                                        paginatedContainerButton
                                            { onPress = Just <| changePageMessage x
                                            , display = String.fromInt x
                                            , active = x == currentPage
                                            , displayBorder = True
                                            }
                                    )
                            )
                        , if lastPageNumberShown < numberOfPages then
                            paginatedContainerButton
                                { onPress =
                                    Just <| changePageMessage <| lastPageNumberShown + 1
                                , display = "..."
                                , active = False
                                , displayBorder = True
                                }

                          else
                            Element.none
                        , paginatedContainerButton
                            { onPress =
                                if currentPage == numberOfPages then
                                    Nothing

                                else
                                    Just <| changePageMessage (currentPage + 1)
                            , display = ">"
                            , active = False
                            , displayBorder = False
                            }
                        ]
               ]
        )
