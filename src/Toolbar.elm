module Toolbar exposing (toolbarElement, toolbarItems)

import Element exposing (Element, el, image, link, row, textColumn)
import ElementLibrary.Style as Style
import Environment exposing (Environment)
import Route exposing (Route(..), hrefString)


toolbarIcon : Route -> Element msg
toolbarIcon route =
    image Style.toolbarIcon <|
        case route of
            ManageFlashcards ->
                { src = "./images/icons/pencil.svg"
                , description = "Manage flashcards"
                }


toolbarItems : List Route
toolbarItems =
    [ ManageFlashcards
    ]


toolbarElements : Environment -> Maybe Route -> List (Element msg)
toolbarElements env activeRoute =
    toolbarItems
        |> List.map
            (\route ->
                link Style.toolbarIcon
                    { url = hrefString env route
                    , label =
                        row
                            (if activeRoute == Just route then
                                Style.activeToolbarItem

                             else
                                Style.toolbarItem
                            )
                            [ el Style.toolbarIcon <| toolbarIcon route
                            ]
                    }
            )


toolbarElement : Environment -> Maybe Route -> Element msg
toolbarElement env activeRoute =
    textColumn
        Style.toolbar
    <|
        toolbarElements env activeRoute
