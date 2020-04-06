module ElementLibrary.Style exposing
    ( MessageType(..)
    , activeToolbarItem
    , button
    , buttonImage
    , dangerText
    , edges
    , errorMessage
    , flashcard
    , heading
    , image
    , infoMessage
    , inputField
    , inputFieldLabel
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
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Label, labelAbove)
import Element.Region as Region



-- HELPERS


edges : { top : Int, bottom : Int, left : Int, right : Int }
edges =
    -- use this for things like Element.paddingEach - override the edges that you need like { edges | bottom = 20 }
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }



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


type MessageType
    = Error
    | Info
    | Successful


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



-- BUTTONS


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


image : List (Attribute msg)
image =
    [ height <| px 36 ]


buttonImage : List (Attribute msg)
buttonImage =
    [ focused [] ]


flashcard : List (Attribute msg)
flashcard =
    [ Font.size 36
    , padding 40
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
