# SoftHaskell

### Program Description

This is a simulation of soft bodies in 2D space using the Gloss library for Haskell.
It simulates a grid of nodes connected by springs which portray the properties of Hooke's Law for spring motion.
The position of each node is updated every frame with Semi-Implicit Euler integration, and is determined from pulling/pushing forces from the connecting springs, and via collision with boundaries and other nodes.

You can interact with the simulation with the standard Gloss mouse motions such as alt-click to rotate, alt + mouse wheel to zoom and left click to move the camera.
Rotating the simulation changes the direction of gravity opposite to your rotation, making a nice effect of the system always falling down your screen.

Simulation constants can be changed within the Constants.hs file, where there is also description on which do what, and the ramifications of changing them.
This file also has display options such as colours, screen size and so forth.
The springs display the force they are applying at one moment by its colour, this visualisation is quite beautiful really, with certain settings you can get some great affects.
I have put some of the settings I think work well in the Constants.hs file already, but please edit them and play with the simulation, I hope you find it as fun as I found it while making this.

The github repo in which this is posted is:
https:/github.com/robbieawest/SoftHaskell

### How to run

The file directory included is a Cabal file structure, so just run
``cabal build``
inside of the directory, provided Cabal and Gloss is installed.

### Dependancies

The simulation is dependant on Gloss and strict-containers.
strict-containers provides strict containers, where I use the Data.Strict.Vector which is autogenerated from Data.Vector.
Strict containers were needed to combat space leaks.

### File Structure

The file structure follows the default cabal file structure, where the main directory contains a CHANGELOG.md, a README.md, a SoftHaskell.cabal file, an /app/ folder where the source files are contained, and a /videos/ folder where the sample output is given.

This program uses multiple source files:
Main.hs (Constains the main :: IO() function)
Constants.hs (Simulation constants)
Simulation.hs (Functions for the simulation)
Base.hs (Misc and drawing functions, definitions of data structures) 

#### Sidenote on sample output

The output given is using constants that I have found produce nice results, changing constants may produce a result which does not look as good or as functional as the ones provided.




