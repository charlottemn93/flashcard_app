module Page.ManageFlashcards exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element, text)
import ElementLibrary.Elements exposing (heading)



-- MODEL


type Model
    = SearchFlashcard


initialModel : Model
initialModel =
    SearchFlashcard



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    heading "Manage flashcards - coming up"
