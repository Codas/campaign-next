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
    { mainFocus : String
    , mapPosition : { zoomLevel: Float , moveX: Float , moveY: Float }
    , mapSize : { height: Float, width: Float }
    }

type DataState = {}

type MapState =
    { activeBackground : Background
    , availableBackgrounds : [Background]
    }

type Background = 
    { name : String
    , url : String
    , height: Float
    , width: Float
    }


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
    }

emptyVisualState : VisualState
emptyVisualState = { mainFocus = "map"
                   , mapPosition = { zoomLevel = 1 , moveX = 0 , moveY = 0 }
                   , mapSize = { width = 1000, height = 500 }
                   }

---- UPDATE -----

data Action
    = NoOp
    | SetMapBackround String
    | SetFocus String
    | MapZoom MapZoomAction
    | MapResized Float Float

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
    MapResized h w ->
        let vState = state.visualState
            mState = vState.mapSize
            changeMapSize ms = { state | visualState <- { vState | mapSize <- ms} }
        in changeMapSize { mState | height <- h, width <- w }
    MapZoom zoomAction ->
        let vState = state.visualState
            mState = vState.mapPosition
            currentZoom = mState.zoomLevel
            changeMapPosition mp = { state | visualState <- { vState | mapPosition <- mp} }
            zoomMap : Float -> State
            zoomMap f =
                let newZoom = currentZoom * f
                    oldX = mState.moveX
                    oldY = mState.moveY
                    vpY = vState.mapSize.height
                    vpX = vState.mapSize.width
                    newX = (f - 1) * vpX * 0.5 - oldX * (f)
                    newY = (f - 1) * vpY * 0.5 - oldY * (f)
                    -- newX = (newZoom - currentZoom) * vpX * 0.5 - oldX * (newZoom - currentZoom)
                    -- newY = (newZoom - currentZoom) * vpY * 0.5 - oldY * (newZoom - currentZoom)
                    newPositions = { mState | zoomLevel <- newZoom, moveX <- -newX, moveY <- -newY }
                in changeMapPosition newPositions
            moveMapX n = changeMapPosition { mState | moveX <- mState.moveX + ( n / (sqrt currentZoom) ) }
            moveMapY n = changeMapPosition { mState | moveY <- mState.moveY + ( n / (sqrt currentZoom) ) }
            zoomFactor = 1.2
        in
        case zoomAction of
          ZoomIn -> zoomMap zoomFactor
          ZoomOut -> zoomMap (1/zoomFactor)
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
          [ ("charSheet", "Character Sheet")
          , ("map", "Map")
          ]
      , node "div" ["className" := "tab-content main hbox space-between"] []
          [ tabPane (inFocus state "charSheet")
              (charSheetHtml state)
          , tabPane (inFocus state "map")
              (mapHtml state)
          ]
      ]

tabBarHtml : State -> [(String, String)] -> Html
tabBarHtml state cs =
    let activeIfFocus f = if inFocus state f then "active" else ""
        linkTag : String -> String -> Html
        linkTag f str = eventNode "a" ["href" := "javascript:void(0);"] []
                             [onclick actions.handle (\_ -> SetFocus f)]
                             [text str] 
        createTabLi : (String, String) -> Html
        createTabLi (f, s) =
            node "li" ["className" := (activeIfFocus f)] []
              [linkTag f s]
    in node "ul" ["className" := "nav nav-tabs", "role" := "tablist"] []
         (map createTabLi cs)

charSheetHtml : State -> [Html]
charSheetHtml state =
    [
      node "div" ["className" := "line"] [] [
        node "dl" ["className" := "character-name"] [] [
          node "dt" [] [] [text "Character name"],
          node "dd" [] [] [text "Geralt of Rivia"]
        ],
        node "dl" ["className" := "character-info"] [] [
          node "dt" [] [] [text "Class & Level"],
          node "dd" [] [] [text "Level 3 Witcher"],
          node "dt" [] [] [text "Background"],
          node "dd" [] [] [text "Monster Hunter"],
          node "dt" [] [] [text "Player Name"],
          node "dd" [] [] [text "Andrzej Sapkowskis"]
        ],
        node "dl" ["className" := "character-info"] [] [
          node "dt" [] [] [text "Race"],
          node "dd" [] [] [text "Human"],
          node "dt" [] [] [text "Alignment"],
          node "dd" [] [] [text "Lawful Neutral"],
          node "dt" [] [] [text "Experience Points"],
          node "dd" [] [] [text "3000"]
        ]
      ],
      node "div" ["className" := "column"] [] [
        node "div" ["className" := "attributes"] [] [
          node "dt" [] [] [text "Strength"],
          node "dd" [] [] [text "14"],
          node "dd" [] [] [text "+2"],
          node "dt" [] [] [text "Dexterity"],
          node "dd" [] [] [text "16"],
          node "dd" [] [] [text "+3"],
          node "dt" [] [] [text "Constitution"],
          node "dd" [] [] [text "16"],
          node "dd" [] [] [text "+3"],
          node "dt" [] [] [text "Intelligence"],
          node "dd" [] [] [text "15"],
          node "dd" [] [] [text "+2"],
          node "dt" [] [] [text "Wisdom"],
          node "dd" [] [] [text "11"],
          node "dd" [] [] [text "+0"],
          node "dt" [] [] [text "Charisma"],
          node "dd" [] [] [text "14"],
          node "dd" [] [] [text "+2"]
        ]
      ],
      node "div" ["className" := "column"] [] [
        node "dl" ["className" := "inspiration"] [] [
          node "dt" [] [] [text "Insipration"],
          node "dd" [] [] [text "No"]
        ],
        node "dl" ["className" := "proficiency-bonus"] [] [
          node "dt" [] [] [text "Proficiency Bonus"],
          node "dd" [] [] [text "+2"]
        ],
        node "dl" ["className" := "saving-throws"] [] [
          node "dt" [] [] [text "Strength"],
          node "dd" [] [] [text "J"],
          node "dd" [] [] [text "+4"],
          node "dt" [] [] [text "Dexterity"],
          node "dd" [] [] [text "N"],
          node "dd" [] [] [text "+4"],
          node "dt" [] [] [text "Constitution"],
          node "dd" [] [] [text "J"],
          node "dd" [] [] [text "+6"],
          node "dt" [] [] [text "Intelligence"],
          node "dd" [] [] [text "N"],
          node "dd" [] [] [text "+2"],
          node "dt" [] [] [text "Wisdom"],
          node "dd" [] [] [text "N"],
          node "dd" [] [] [text "+0"],
          node "dt" [] [] [text "Charisma"],
          node "dd" [] [] [text "N"],
          node "dd" [] [] [text "+2"],
          node "dt" [] [] [text "Saving Throws"]
        ]
      ]
    ]

mapHtml : State -> [Html]
mapHtml state =
    let mState = state.mapState
        vmState = state.visualState.mapPosition
        bg = mState.activeBackground
        mapBG = "url('" ++ bg.url ++"')"
        scaleFactor = vmState.zoomLevel
        translateX = px vmState.moveX
        translateY = px vmState.moveY
        translate = "translate(" ++ translateX ++ ", " ++ translateY ++ ")"
        scale = "scale(" ++ (show scaleFactor) ++")"
        transform = join " " [translate, scale]
    in 
    [ node "div" ["className" := "map", "id" := "map-container"] []
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
inFocus : State -> String -> Bool
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
state = foldp step startingState signals

signals = merge actions.signal mapResizedActions

-- load state from local storage or from the server
startingState : State
startingState = Maybe.maybe emptyState id getStorage

-- actions from user input. Initialized as NoOp
actions : Input Action
actions = Input.input NoOp

mapResizedActions : Signal Action
mapResizedActions = lift (\(h, w) -> MapResized h w) getMapElementSize

-- interactions with localStorage to save app state (type alias support coming soon!)
port getStorage : Maybe 
         { dataState: {}
         , mapState: { activeBackground:      { name: String, url: String, height: Float, width: Float }
                     , availableBackgrounds: [{ name: String, url: String, height: Float, width: Float }]
                     }
         , visualState: { mainFocus: String
                        , mapPosition: { zoomLevel: Float , moveX: Float , moveY: Float }
                        , mapSize: { height: Float, width: Float }
                        }
         }

port setStorage : Signal 
         { dataState: {}
         , mapState: { activeBackground:      { name: String, url: String, height: Float, width: Float }
                     , availableBackgrounds: [{ name: String, url: String, height: Float, width: Float }]
                     }
         , visualState: { mainFocus: String
                        , mapPosition : { zoomLevel: Float , moveX: Float , moveY: Float }
                        , mapSize: { height: Float, width: Float }
                        }
         }
port setStorage = state

port getMapElementSize : Signal (Float, Float)

port log : Signal String
port log = lift show getMapElementSize                 

-- WebSocket connection to the server for actions transmissions
-- socket = WebSocket.connect "ws://127.0.0.1:3000" (constant "no name")

-- why is undefined not there by default..?        
undefined = undefined
