module ElementLibrary.Style exposing
    ( activeToolbarItem
    , button
    , buttonImage
    , dangerButton
    , dangerText
    , errorMessage
    , flashcard
    , flashcardNoPadding
    , heading
    , image
    , infoMessage
    , inputField
    , inputFieldLabel
    , linkColour
    , mutedColour
    , searchField
    , searchResultButton
    , shuffleImage
    , successfulMessage
    , toolbar
    , toolbarIcon
    , toolbarItem
    )

import Element
    exposing
        ( Attr
        , Attribute
        , Color
        , alignTop
        , centerX
        , fill
        , focused
        , height
        , mouseOver
        , padding
        , paddingEach
        , paddingXY
        , pointer
        , px
        , rgb255
        , rgba255
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Label, labelAbove)
import Element.Region as Region
import ElementLibrary.Helpers exposing (edges)
import Html.Events
import Json.Decode as Decode



-- HELPERS


defaultImageHeight : Int
defaultImageHeight =
    36



-- COLOURS


mutedColour : Color
mutedColour =
    rgb255 100 122 132


linkColour : Color
linkColour =
    rgb255 51 122 183



-- HEADINGS


heading : List (Attribute msg)
heading =
    [ Region.heading 1
    , Font.size 48
    , Font.color <| rgb255 0 173 238
    , paddingEach
        { edges
            | bottom = 10
        }
    , centerX
    ]



-- DISPLAY ONLY


inputFieldLabel : String -> Label msg
inputFieldLabel labelText =
    labelAbove
        [ Font.size 14
        , Font.bold
        , paddingEach
            { edges
                | bottom = 5
                , top = 10
            }
        ]
        (text labelText)


dangerText : List (Attribute msg)
dangerText =
    [ width fill
    , paddingXY 0 3
    , Font.size 14
    , height <| px 10
    , Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]
    , Font.color <| rgb255 169 68 66
    ]


messageStyles : List (Attribute msg)
messageStyles =
    [ Border.widthEach
        { left = 3
        , top = 0
        , bottom = 0
        , right = 0
        }
    , Font.size 14
    , width fill
    , padding 20
    ]


errorMessage : List (Attribute msg)
errorMessage =
    messageStyles
        ++ [ Region.announceUrgently
           , Background.color <| rgb255 242 222 222
           , Border.color <| rgb255 235 204 209
           , Font.color <| rgb255 169 68 66
           ]


infoMessage : List (Attribute msg)
infoMessage =
    messageStyles
        ++ [ Region.announce
           , Background.color <| rgb255 217 237 247
           , Border.color <| rgb255 188 232 241
           , Font.color <| rgb255 49 112 143
           ]


successfulMessage : List (Attribute msg)
successfulMessage =
    messageStyles
        ++ [ Region.announceUrgently
           , Background.color <| rgb255 223 240 216
           , Border.color <| rgb255 60 118 61
           , Font.color <| rgb255 60 118 61
           ]



-- INPUT FIELDS


inputField : List (Attribute msg)
inputField =
    [ width fill
    , Font.size 14
    , Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]
    ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


searchField : Maybe msg -> List (Attribute msg)
searchField onEnterMsg =
    case onEnterMsg of
        Nothing ->
            inputField

        Just m ->
            inputField ++ [ onEnter m ]



-- BUTTONS


searchResultButton : List (Attribute msg)
searchResultButton =
    [ width fill
    , Border.dashed
    , Border.widthEach { edges | bottom = 1 }
    , Border.color <| rgb255 0 0 0
    , paddingXY 0 10
    , spacing 5
    ]


button : Bool -> List (Attribute msg)
button disabled =
    [ Font.color <| rgb255 255 255 255
    , Font.size 14
    , padding 10
    , Border.shadow
        { blur = 3
        , color = rgba255 0 0 0 0.2
        , offset = ( 0, 1 )
        , size = 1
        }
    , if disabled == True then
        Background.color <| rgba255 51 122 183 0.4

      else
        Background.color <| rgb255 51 122 183
    ]


dangerButton : Bool -> List (Attribute msg)
dangerButton disabled =
    [ Font.color <| rgb255 255 255 255
    , Font.size 14
    , padding 10
    , Border.shadow
        { blur = 3
        , color = rgba255 0 0 0 0.2
        , offset = ( 0, 1 )
        , size = 1
        }
    , if disabled == True then
        Background.color <| rgba255 217 83 79 0.4

      else
        Background.color <| rgb255 217 83 79
    ]


shuffleImageHighlight : List (Attr decorative msg)
shuffleImageHighlight =
    [ Border.glow (rgb255 46 244 41) 3.0
    ]


customShuffleImage : List (Attribute msg)
customShuffleImage =
    [ height <| px 36
    , paddingEach
        { edges
            | top = 5
            , bottom = 40
            , left = 10
            , right = 10
        }
    , Border.rounded 100
    , pointer
    ]


shuffleImage : Bool -> List (Attribute msg)
shuffleImage shuffleOn =
    if shuffleOn then
        customShuffleImage ++ shuffleImageHighlight

    else
        customShuffleImage


image : Maybe Int -> List (Attribute msg)
image pixels =
    case pixels of
        Just p ->
            [ height <| px p ]

        Nothing ->
            [ height <| px defaultImageHeight ]


buttonImage : List (Attribute msg)
buttonImage =
    [ focused [] ]


flashcard : Bool -> List (Attribute msg)
flashcard isEditable =
    [ if isEditable then
        padding 5

      else
        padding 40
    , Font.size 36
    , centerX
    , Border.shadow
        { blur = 3
        , color = rgba255 0 0 0 0.2
        , offset = ( 0, 1 )
        , size = 1
        }
    , Background.color <| rgb255 255 255 136
    ]


flashcardNoPadding : List (Attribute msg)
flashcardNoPadding =
    [ Font.size 36
    , centerX
    , Border.shadow
        { blur = 3
        , color = rgba255 0 0 0 0.2
        , offset = ( 0, 1 )
        , size = 1
        }
    , Background.color <| rgb255 255 255 136
    ]



-- TOOLBAR


toolbar : List (Attribute msg)
toolbar =
    [ height <| px 50
    , width fill
    , Background.color <| rgb255 51 122 183
    , Font.color <| rgb255 153 153 153
    , Font.size 16
    , Region.navigation
    , spacingXY 50 0
    ]


toolbarIcon : List (Attribute msg)
toolbarIcon =
    [ alignTop
    , height <| px 32
    ]


toolbarHighlight : List (Attr decorative msg)
toolbarHighlight =
    [ Background.color <| rgb255 51 122 255
    ]


activeToolbarItem : List (Attribute msg)
activeToolbarItem =
    toolbarItem ++ toolbarHighlight


toolbarItem : List (Attribute msg)
toolbarItem =
    [ width fill
    , paddingXY 18 7
    , spacingXY 20 20
    , pointer
    , mouseOver toolbarHighlight
    , focused toolbarHighlight
    ]
