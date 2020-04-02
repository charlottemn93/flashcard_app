module ElementLibrary.Style exposing (ButtonType(..), button)

import Element
    exposing
        ( Attribute
        , padding
        , rgb255
        , rgba255
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


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
