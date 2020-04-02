module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element, text)


type Model
    = SomeModel


type Msg
    = NoOp


initialModel : ( Model, Cmd Msg )
initialModel =
    ( SomeModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Element msg
view _ =
    text "Manage flashcards page"
