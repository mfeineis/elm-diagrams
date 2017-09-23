module Diagrams.Wiring exposing (..)

{-| Functions and types for getting a (possibly interactive) diagram onto the screen.

# Types
@docs CollageLocation, PrimMouseEvent, PrimMouseEvtType, CollageLocFunc

# Functions
@docs makeUpdateStream, mouseEvents, offsetMousePos

# Input Signals
@docs floatMousePos, floatWindowDims, toPoint
-}

import Mouse
import Window
import Collage as C

import Diagrams.Geom exposing (..)

{-| Position of a rectangle on the screen in which a diagram will be drawn (as a `Graphics.Collage`).
Offset is difference between top left of screen and top left of collage, increasing right and down. -}
type alias CollageLocation =
  OffsetDimsBox

{-|-}
type alias PrimMouseEvent =
  (PrimMouseEvtType, Point)

{-|-}
type PrimMouseEvtType
  = MouseUpEvt
  | MouseDownEvt
  | MouseMoveEvt

{-| Given window size, where on screen and how big is your collage? -}
type alias CollageLocFunc =
  Dims -> CollageLocation

--makeUpdateStream : CollageLocFunc -> Sub (CollageLocation, PrimMouseEvent)
--makeUpdateStream clf =
--    let predicate = \(loc, (evtType, point)) -> pointInside point { loc | offset = (0, 0) }
--        mapDims dim =
--            let collageLoc = clf dim
--                mapMouse evt =
--                    case predicate (collageLoc, evt) of
--                        True ->
--                            (collageLoc, evt)
--
--                        False ->
--                            (collageLoc, evt)
--
--            in Sub.map mapMouse (mouseEvents collageLoc)
--
--    in Sub.map mapDims floatWindowDims
--
--    --let collageLocs = Sub.map clf floatWindowDims
--    --    mouseEvts = mouseEvents collageLocs
--    --    predicate = \(loc, (evtType, point)) -> pointInside point { loc | offset = (0, 0) }
--    --in case predicate  of
--    --    True ->
--    --    False ->
--    --        Sub.none


---- BUG: `pointInside` sees offset as middle of box; `Wiring` uses it as top left (#37)
--{-| Given collage location function, return stream of (collage location, mouse event)
--pair s, where mouse coordinates are relative to the center of the collage at its present
--location, and increasing up and to the right. -}
--makeUpdateStream : CollageLocFunc -> Signal (CollageLocation, PrimMouseEvent)
--makeUpdateStream clf =
--    let collageLocs = S.map clf floatWindowDims
--        mouseEvts = mouseEvents collageLocs
--        tups = S.map2 (,) collageLocs mouseEvts
--    in S.filter (\(loc, (evtType, point)) -> pointInside point { loc | offset = (0, 0)})
--                ({dims={width=0, height=0}, offset=(0,0)},(MouseUpEvt,(0,0)))
--                tups

type Msg
    = MouseOnCollage CollageLocation PrimMouseEvent
    | MousePosition Point
    | NoOp
    | WindowDimension Dims

makeUpdateStream : CollageLocFunc -> Sub Msg
makeUpdateStream clf =
    let x dim =
        let collageLocs = clf dim

    in Sub.map x floatWindowDims


--{-| Given a signal of collage locations, return a signal of mouse events offset from the
--center of that location. -}
--mouseEvents : Signal CollageLocation -> Signal PrimMouseEvent
--mouseEvents loc =
--    let upDown = S.map (\down -> if down then MouseDownEvt else MouseUpEvt) Mouse.isDown
--        moves = S.map (always MouseMoveEvt) Mouse.position
--        events = S.merge upDown moves
--        adjustedMousePos = S.map2 offsetMousePos loc floatMousePos
--    in S.map2 (,) events adjustedMousePos

mouseEvents : CollageLocation -> Sub PrimMouseEvent
mouseEvents loc =
    let transform = offsetMousePos loc
    in Sub.batch
        [ Sub.map (\p -> ( MouseDownEvt, transform p )) (Mouse.downs toPoint)
        , Sub.map (\p -> ( MouseUpEvt, transform p )) (Mouse.ups toPoint)
        , Sub.map (\p -> ( MouseMoveEvt, transform p )) (Mouse.moves toPoint)
        ]

{-| Given the position of the top-left of a collage (from the top-left of the screen; coords increasing right and down)
and the dimensions of the collage, return a signal of the mouse position relative to the center of that collage,
and increasing up and to the right instead of down and to the right. -}
offsetMousePos : CollageLocation -> Point -> Point
offsetMousePos loc (x, y) = let (offsetX, offsetY) = loc.offset
                                {width, height} = loc.dims
                            in (x - width/2 - offsetX, (height/2 + offsetY) - y)

-- input signals

{-|-}
toPoint : Mouse.Position -> Point
toPoint {x, y} = (toFloat x, toFloat y)

--{-|-}
--floatMousePos : Signal Point
--floatMousePos = S.map toPoint Mouse.position

--floatMousePos : Sub Point
--floatMousePos =
--    Mouse.moves toPoint

floatMousePos : Sub Msg
floatMousePos =
    Mouse.moves (MousePosition << toPoint)

floatWindowDims : Sub Msg
floatWindowDims =
    Window.resizes (WindowDimension << (\{ width, height } -> { width = toFloat width, height = toFloat height }))


--floatWindowDims : Sub Dims
--floatWindowDims =
--    Window.resizes (\{ width, height } -> { width = toFloat width, height = toFloat height })

--{-|-}
--floatWindowDims : Signal Dims
--floatWindowDims = S.map (\(w, h) -> { width = toFloat w, height = toFloat h })
--                        Window.dimensions
