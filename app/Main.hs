module Main where

import Prelude hiding (replicate, head, tail, map)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.Strict.Vector as V

import Simulation
import Structure
import Base


screenX :: Float
screenX = 1050.0

screenY :: Float 
screenY = 900.0

structureWidth :: Int
structureWidth = 10

structureHeight :: Int
structureHeight = 10

nodeMass :: Float 
nodeMass = 1.0

--This is the scalar of the gravity vector applied to the velocity every frame
gravity :: Float
gravity = 1.2

radius :: Float
radius = 9.0

--This radius is seperate as it defines the radius at which the nodes should collide with eachover.
--This does not include boundary collision.
--They have been separated to give more design control over the nodes.
collisionRadius :: Float
collisionRadus = 13.0

damping :: Float
damping = 10.0

springConstant :: Float
springConstant = 15.0

anchorLength :: Float
anchorLength = 35.0

frictionConstant :: Float
frictionConstant = 0.01

--The offset of the top left of the structure
--Note that the coordinates are based off of the top left of the boundaries, which is different from default gloss.
origin :: (Float, Float)
origin = (150.0, 150.0)


main :: IO ()
main = simulate
            (InWindow "SoftHaskell"
                        (floor screenX + 300, floor screenY)
                        (10, 10))
            black
            144
            (initial
                structureWidth
                structureHeight
                nodeMass
                gravity
                radius
                collisionRadius
                damping
                springConstant
                anchorLength
                origin)
            draw
            step
