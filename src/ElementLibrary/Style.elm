module ElementLibrary.Style exposing (ButtonType(..), MessageType(..), button, errorMessage, globalLayout, infoMessage, inputField, successfulMessage)

import Element
    exposing
        ( Attribute
        , fill
        , padding
        , rgb255
        , rgba255
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region



-- GLOBAL


globalLayout : List (Attribute msg)
globalLayout =
    [ Font.size 14
    ]



-- DISPLAY ONLY


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


type ButtonType
    = Danger
    | Success
    | Primary


button : { buttonType : ButtonType, disabled : Bool } -> List (Attribute msg)
button { buttonType, disabled } =
    [ Font.color <| rgb255 255 255 255
    , Font.size 14
    , padding 10
    , Border.shadow
        { blur = 3
        , color = rgba255 0 0 0 0.2
        , offset = ( 0, 1 )
        , size = 1
        }
    ]
        ++ (case buttonType of
                Danger ->
                    [ if disabled == True then
                        Background.color <| rgba255 217 83 79 0.4

                      else
                        Background.color <| rgb255 217 83 79
                    ]

                Success ->
                    [ if disabled == True then
                        Background.color <| rgba255 161 205 58 0.4

                      else
                        Background.color <| rgb255 161 205 58
                    ]

                Primary ->
                    [ if disabled == True then
                        Background.color <| rgba255 51 122 183 0.4

                      else
                        Background.color <| rgb255 51 122 183
                    ]
           )
