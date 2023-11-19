module Main where

import Prelude hiding (replicate, head, tail, map)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.Strict.Vector as V

import Simulation
import Base
import Constants



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
                massC
                gravityC
                radiusC
                collisionRadiusC
                dampingC
                springConstantC
                anchorLengthC
                origin)
            draw
            step
