module Pi where

import String
import List exposing (..)
import Random
import Time
import Signal
import Color
import Graphics.Element exposing (Element)
import Graphics.Collage as Collage
import Window

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

upstate : Point -> State -> State
upstate p (h, m) =
   let {x, y}   = p
       (hn, hs) = h
       (mn, ms) = m
   in if sqrt (x^2 + y^2) < 1.0 then
         ((hn + 1, p::hs), m)
      else
         (h, (mn + 1, p::ms))

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s1 =
   let g       = Random.float -1 1
       (x, s2) = Random.generate g s1
       (y, s3) = Random.generate g s2
   in ({x=x, y=y}, s3)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
   let seed          = genPoint <| Random.initialSeed 0
       next _ (_, s) = genPoint s
   in Time.every 100 |> Signal.foldp next seed

signalPoint : Signal Point
signalPoint = signalPointSeed |> Signal.map fst

view : (Int, Int) -> State -> Element
view (width, height) state =
   let ((_, hits), (_, misses)) = state
       radius = toFloat height / 2 - 10
       circle =
         Collage.circle radius |>
         Collage.outlined (Collage.solid Color.blue)
       square =
         Collage.square (radius*2) |>
         Collage.outlined (Collage.solid Color.red)
       circles {x, y} =
         Collage.circle 2 |>
         Collage.filled Color.blue |>
         Collage.move (x * radius, y * radius)
       squares {x, y} =
         Collage.square 4 |>
         Collage.filled Color.red |>
         Collage.move (x * radius, y * radius)
   in Collage.collage width height <|
      [circle, square] ++ map circles hits ++ map squares misses

main : Signal Element
main =
  Signal.map2 view Window.dimensions <|
  Signal.foldp upstate initState signalPoint
