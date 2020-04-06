module Toolbar exposing (toolbarElement, toolbarItems)

import Element exposing (Element, el, image, link, row)
import ElementLibrary.Style as Style
import Environment exposing (Environment)
import Route exposing (Route(..), hrefString)


toolbarIcon : Route -> Maybe (Element msg)
toolbarIcon route =
    case route of
        Revise ->
            Just <|
                image Style.toolbarIcon <|
                    { src = "./images/icons/home.svg"
                    , description = "Revise"
                    }

        ManageFlashcards ->
            Just <|
                image Style.toolbarIcon <|
                    { src = "./images/icons/pencil.svg"
                    , description = "Manage flashcards"
                    }

        MyAccount ->
            Just <|
                image Style.toolbarIcon <|
                    { src = "./images/icons/user.svg"
                    , description = "My account"
                    }

        PageNotFound ->
            Nothing


toolbarItems : List Route
toolbarItems =
    [ Revise
    , ManageFlashcards
    , MyAccount
    ]


toolbarElements : Environment -> Maybe Route -> List (Element msg)
toolbarElements env activeRoute =
    toolbarItems
        |> List.map
            (\route ->
                link Style.toolbarIcon
                    { url = hrefString env route
                    , label =
                        el
                            (if activeRoute == Just route then
                                Style.activeToolbarItem

                             else
                                Style.toolbarItem
                            )
                        <|
                            case toolbarIcon route of
                                Just icon ->
                                    icon

                                Nothing ->
                                    Element.none
                    }
            )


toolbarElement : Environment -> Maybe Route -> Element msg
toolbarElement env activeRoute =
    row Style.toolbar <|
        toolbarElements env activeRoute
