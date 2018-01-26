module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


width::Int
width = 640

height :: Int
height = 480

window::Display
window = InWindow "Haskellossy" (width, height) (10, 10) 



data WorldState = Game 
  { objectsLocation :: [(Float, Float)]
  , objectsVelocity :: [(Float, Float)]
  } deriving Show

initialState :: WorldState
initialState = Game
  { objectsLocation = [(-10, 230), (-40, 40)]
  , objectsVelocity = [(250, -190), (50,50)]
  }


renderWorldState :: WorldState -> Picture
renderWorldState game = 
  pictures [object]
  where 
    object = mkObject (objectsLocation game)
    mkObject :: [(Float, Float)] -> Picture
    mkObject [] = pictures []
--      where obj = uncurry translate a $ color red $ circleSolid 10
    mkObject (a:as) = pictures [ obj ,mkObject as ]
      where obj = uncurry translate a $ color red $ circleSolid 10




moveObject :: Float -> WorldState -> WorldState
moveObject sec game = game { objectsLocation =  ttt locations velocities } 
  where 
    ttt :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
    ttt [] [] = []
    ttt ( (ax, ay) : (ls) ) ( (vx, vy) : (vs) ) = [(ax + vx * sec, ay + vy * sec)] ++ ttt ls vs
    locations = objectsLocation game
    velocities = objectsVelocity game



fps ::Int
fps = 60

type Radius = Float
type Position = (Float, Float)

topBottomCollision :: Position -> Radius -> Bool
topBottomCollision (_, y) radius = topCollision || bottomCollision
  where 
    topCollision = y - radius <= -fromIntegral height/2
    bottomCollision = y + radius >= fromIntegral height/2


leftRightCollision :: Position -> Radius -> Bool
leftRightCollision (x, _) radius = leftCollision || rightCollision
  where 
    leftCollision = x - radius <= -fromIntegral width/2
    rightCollision = x + radius >= fromIntegral width/2


wallBounce :: WorldState -> WorldState
wallBounce game = game { objectsVelocity = checkCollision locations velocities
                       , objectsLocation = locations
                       }
  where 
    velocities = objectsVelocity game
    locations = objectsLocation game
    radius = 10
    checkCollision :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
    checkCollision [] [] = []
    checkCollision ((lx, ly):ls) ((vx, vy):vs) = 
      (vx', vy') : checkCollision ls vs
        where
	    vy' = if topBottomCollision (lx, ly) radius
	      then -vy
	      else vy
	    vx' = if leftRightCollision (lx, ly) radius
	      then -vx
	      else vx
	    

update :: ViewPort -> Float -> WorldState -> WorldState
update _ sec = wallBounce . moveObject sec


main :: IO ()
main = simulate window blue fps initialState renderWorldState update
 
