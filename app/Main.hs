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
                        (floor screenX + 100, floor screenY + 100) --Extra offset gives a border
                        (10, 10))
            (makeColor br bg bb 1.0)
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
    where (br, bg, bb) = backgroundColor
