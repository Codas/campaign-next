import Mouse
import WebSocket
import String
import Html
import Html (..)
import Html.Events (..)
import Html.Optimize.RefEq as Ref
import Maybe
import Window

import Graphics.Input (..)
import Graphics.Input as Input

-- Model
-- The full application state of the campaign manager app
type State =
    { dataState   : DataState
    , visualState : VisualState
    }

type VisualState =
    { mainFocus : FocusArea
    }

type DataState =
    { map : Map
    }

type Map =
    { backgroundUrl : String
    }

data FocusArea
    = CharacterSheet
    | Map

setBackround : String -> Map -> Map
setBackround url map = { map | backgroundUrl <- url } 

emptyState : State
emptyState =
    { dataState = { map = { backgroundUrl = ""}}  
    , visualState = {}   
    }


main : Signal Element
main = lift (flow down) (foldp step [(asText "connecting ...")] socket)

step : String -> [Element] -> [Element]
step a b = (b ++ [asText (show a)])

socket = WebSocket.connect "ws://127.0.0.1:3000" (constant "no name")