module ElementLibrary.Helpers exposing (MessageType(..), edges)


type MessageType
    = Error
    | Info
    | Successful


edges : { top : Int, bottom : Int, left : Int, right : Int }
edges =
    -- use this for things like Element.paddingEach - override the edges that you need like { edges | bottom = 20 }
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }
