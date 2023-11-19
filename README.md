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


