module Page.MyAccount exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element, text)
import ElementLibrary.Elements exposing (heading)



-- Model


type Model
    = Ready


initialModel : Model
initialModel =
    Ready



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    heading "My Account - coming up"
