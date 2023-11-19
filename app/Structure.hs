module Structure where

{-
This file defines the constants of the simulation

By all means edit these and find interesting combinations
Be aware that the simulation may be instable upon certain conditions, or simply will not function, for example:

Defining a collision radius greater than or equal to half of the anchor length, this causes the nodes to be
 unable to move.

Defining a collision radius too small for the anchor length, an appropriate collision radius is about 40% of the
 anchor length. A small collision radius causes the structure to collapse inside of itself. Note that this is not
 exactly an issue with the simulation, but an inherant limitation with the cross-hatch node and spring structure.
 (Using the standard node-node collision system that is)

A damping constant too low for the spring constant, causing an explotion in spring forces. This explosion is a
 result of the integration technique being used(1), whereas more stable integration techniques(2) require more
 computing power.
 (1) Integration techniques are used due to the limited number of frames per second, if we had an unlimited fps,
     and a delta time of zero, we would have no issues. 
     The current technique is semi-implicit euler, which is a fancy way of saying update velocity first then
      position. Euler refers to the Euler method of approximating a first order differential equation.
 (2) More advanced techniques such as Runge-Kutte 2 or 4 provide a better approximation, but are more expensive.
     Verlet integration does however exist, which is more stable without a difference in computation, but for
     this project it is not exactly needed.

Constants very high or very small, these values can be exacerbated by numerical instability and may lead to 
 instability.
-}