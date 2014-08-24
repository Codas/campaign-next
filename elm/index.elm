module Index where

import Mouse
import WebSocket
import String
import Html
import Html (..)
import Html.Events (..)
import Html.Optimize.RefEq as Ref
import Maybe
import Window
import Debug

import Graphics.Input (..)
import Graphics.Input as Input

-- Model
-- The full application state of the campaign manager app
type State =
    { dataState: DataState
    , mapState: MapState
    , visualState: VisualState
    }

type VisualState =
    { mainFocus : FocusArea
    }

type DataState = {}

type MapState =
    { activeBackground : Background
    , availableBackgrounds : [Background]
    , zoomLevel: Float
    , moveX: Float
    , moveY: Float
    }

type Background = 
    { name : String
    , url : String
    , height: Float
    , width: Float
    }

    
data FocusArea
    = FocusCharSheet
    | FocusMap

emptyState : State
emptyState =
    { dataState = emptyDataState
    , mapState = emptyMapState
    , visualState = emptyVisualState
    }

emptyDataState : DataState
emptyDataState = { }

emptyMapState : MapState
emptyMapState =
    let faerunBackground =
            { name = "Faerun"
            , url = "static/img/map/faerun.jpg"
            , height = 3030
            , width = 4317
            }
    in
    { activeBackground = faerunBackground
    , availableBackgrounds = [ faerunBackground ]
    , zoomLevel = 1
    , moveX = 0
    , moveY = 0
    }

emptyVisualState : VisualState
emptyVisualState =
    { mainFocus = FocusMap }

---- UPDATE -----

data Action
    = NoOp
    | SetMapBackround String
    | SetFocus FocusArea
    | MapZoom MapZoomAction

data MapZoomAction
    = ZoomIn
    | ZoomOut
    | MoveUp
    | MoveRight
    | MoveDown
    | MoveLeft
    
-- update Map
setBackround : Background -> MapState -> MapState
setBackround bg map = { map | activeBackground <- bg }

-- How we step the state forward for any given action
step : Action -> State -> State
step action state =
  case action of
    NoOp -> state
    SetFocus f ->
        let vState = state.visualState
        in { state | visualState <- { vState | mainFocus <- f } } 
    MapZoom zoomAction ->
        let mState = state.mapState
            currentLevel = mState.zoomLevel
            zoomMap : Float -> State
            zoomMap f = { state | mapState <- { mState | zoomLevel <- (currentLevel * f) } }
            moveMapX n = { state | mapState <- { mState | moveX <- (mState.moveX + n) } }
            moveMapY n = { state | mapState <- { mState | moveY <- (mState.moveY + n) } }
        in
        case zoomAction of
          ZoomIn -> zoomMap 1.1
          ZoomOut -> zoomMap 0.9
          MoveUp -> moveMapY 100
          MoveRight -> moveMapX -100
          MoveDown -> moveMapY -100
          MoveLeft -> moveMapX 100
    _ -> state
                      
---- VIEW ----

-- convert state model to virtual html representation
view : State -> Html
view state =
    node "div" ["className" := "vbox viewport"] []
      [ tabBarHtml state
          [ (FocusCharSheet, "Character Sheets")
          , (FocusMap, "Map")
          ]
      , node "div" ["className" := "tab-content main hbox space-between"] []
          [ tabPane (inFocus state FocusCharSheet)
              (charSheetHtml state)
          , tabPane (inFocus state FocusMap)
              (mapHtml state)
          ]
      ]

tabBarHtml : State -> [(FocusArea, String)] -> Html
tabBarHtml state cs =
    let activeIfFocus f = if inFocus state f then "active" else ""
        linkTag : FocusArea -> String -> Html
        linkTag f str = eventNode "a" ["href" := "javascript:void(0);"] []
                             [onclick actions.handle (\_ -> SetFocus f)]
                             [text str] 
        createTabLi : (FocusArea, String) -> Html
        createTabLi (f, s) =
            node "li" ["className" := (activeIfFocus f)] []
              [linkTag f s]
    in node "ul" ["className" := "nav nav-tabs", "role" := "tablist"] []
         (map createTabLi cs)

charSheetHtml : State -> [Html]
charSheetHtml state =
    [text "CharSheet"]

mapHtml : State -> [Html]
mapHtml state =
    let mState = state.mapState
        bg = mState.activeBackground
        mapBG = "url('" ++ bg.url ++"')"
        scaleFactor = mState.zoomLevel
        translateX = px mState.moveX
        translateY = px mState.moveY
        translate = "translate(" ++ translateX ++ ", " ++ translateY ++ ")"
        scale = "scale(" ++ (show scaleFactor) ++")"
        transform = join " " [translate, scale]
    in 
    [ node "div" ["className" := "map"] []
        [ node "div" ["className" := "map-controls"] []
            [ node "div" [] []
                [ eventNode "a" ["href" := "javascript:void(0)", "className" := "control fa fa-plus"] []
                    [onclick actions.handle (\_ -> MapZoom ZoomIn)] []
                , eventNode "a" ["href" := "javascript:void(0)", "className" := "control fa fa-minus"] []
                    [onclick actions.handle (\_ -> MapZoom ZoomOut)] []
                , eventNode "a" ["href" := "javascript:void(0)", "className" := "control fa fa-arrow-up"] []
                    [onclick actions.handle (\_ -> MapZoom MoveUp)] []
                , eventNode "a" ["href" := "javascript:void(0)", "className" := "control fa fa-arrow-down"] []
                    [onclick actions.handle (\_ -> MapZoom MoveDown)] []
                , eventNode "a" ["href" := "javascript:void(0)", "className" := "control fa fa-arrow-left"] []
                    [onclick actions.handle (\_ -> MapZoom MoveLeft)] []
                , eventNode "a" ["href" := "javascript:void(0)", "className" := "control fa fa-arrow-right"] []
                    [onclick actions.handle (\_ -> MapZoom MoveRight)] []
                ]
            ]
        , node "div" ["className" := "map-background"]
            [ "backgroundImage" := mapBG
            , "height" := px bg.height
            , "width" := px bg.width
            , "transform" := transform
            ] []
        ]
    ]

---- BOOTSTRAP HTML HELPER ---
row : String -> [Html] -> Html
row s cs = node "div" ["className" := "hbox" ++ s] [] cs
         
maxRow : String -> [Html] -> Html
maxRow s cs = node "section" ["className" := s] [] cs
         
col : String -> [Html] -> Html
col s cs = node "div" ["className" := "flex-" ++ s] [] cs

tabLink : String -> Bool -> Html
tabLink = undefined

tabPane : Bool -> [Html] -> Html
tabPane b cs =
    let active = if b then "active" else ""
    in node "article" ["className" := "tab-pane " ++ active] [] cs

iconButton button icon events =
    eventNode "a" ["href" := "javascript:void(0);", "className" := "btn btn-sm btn-" ++ button] [] events
      [ node "i" ["className" := "fa fa-lg fa-" ++ icon] [] [] ]

btnGroup : [Html] -> Html
btnGroup cs = node "div" ["className" := "btn-group"] [] cs


---- HTML AND STATE HELPER ---
inFocus : State -> FocusArea -> Bool
inFocus state focus = state.visualState.mainFocus == focus


---- INPUTS ----

-- wire the application together
main : Signal Element
main = lift2 scene state Window.dimensions

-- convert state and window dimensions to displayable element
scene : State -> (Int,Int) -> Element
scene state (w,h) =
    container w h midTop (Html.toElement w h (view state))

-- manage the state of our application over time
state : Signal State
state = foldp step startingState actions.signal

-- load state from local storage or from the server
startingState : State
startingState = emptyState

-- actions from user input. Initialized as NoOp
actions : Input Action
actions = Input.input NoOp


-- WebSocket connection to the server for actions transmissions
-- socket = WebSocket.connect "ws://127.0.0.1:3000" (constant "no name")

-- why is undefined not there by default..?        
undefined = undefined