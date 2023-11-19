module Simulation where

import Prelude hiding (replicate, head, tail, map)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.StrictV.Vector as V

import Base

--Stepping functions
step :: ViewPort -> Float -> Simulation -> Simulation
step view dt sim = collisionRes
    where
        physicsRes = physicsStep sim dt view
        collisionRes = seq physicsRes (collisionStep physicsRes dt)

collisionStep :: Simulation -> Float -> Simulation
collisionStep (Simulation nodes springs) dt = seq nodesCollisionRes (Simulation nodesCollisionRes springs)
    where
        nodesCollisionRes = seq boundaryRes (nodeCollisions boundaryRes 0)
        boundaryRes = (map.map) boundaryCollision nodes



addGravity :: Float -> ViewPort -> Node -> Node
addGravity dt view (Node m g r c pp p v) = Node m g r c pp (p `pv` newV) newV
    where
        theta = viewPortRotate view * (pi / 180.0) 
        gravVec = (sin theta, cos theta)
        newV = v `pv` (gravVec `mv` (dt * g))

physicsStep :: Simulation -> Float -> ViewPort -> Simulation
physicsStep (Simulation nodes springs) dt view = seq hookesRes (Simulation hookesRes springs)
    where
        hookesRes = seq gravRes (springPhysics gravRes springs dt) --Calculating spring forces
        gravRes = (map.map) (addGravity dt view) nodes --Calculating gravity forces

--Spring physics functions

springPhysics :: Vector (Vector Node) -> Vector Spring -> Float -> Vector (Vector Node)
springPhysics nodes springs dt 
    | V.null springs = nodes
    | otherwise = seq physicsRes (springPhysics physicsRes other dt) 
    where
        physicsRes = physics nodes spring dt
        spring = head springs
        other = tail springs

--Calculates spring force for a single spring
physics :: Vector (Vector Node) -> Spring -> Float -> Vector (Vector Node)
physics nodes (Spring damping springConstant anchorLength n1 n2) dt
    | len == 0 = nodes
    | otherwise = res
    where
        (n1i, n1j) = n1
        (n2i, n2j) = n2
        (Node m g r c pp p v) = nodes ! n1i ! n1j --Index nodes
        (Node m1 g1 r1 c1 pp1 p1 v1) = nodes ! n2i ! n2j

        (diffx, diffy) = p1 `sv` p
        theta = atan2 diffy diffx
        len = pythag (diffx, diffy)

        tension = springConstant * (len - anchorLength) --Hooke' law
        forcePer = (tension / 2.0 * cos theta, tension / 2.0 * sin theta)
        --Node 1 has -forcePer acting, and forcePer for Node 2 since diff = node2 - node1 (From node1 pointing to node2)

        normDiff = (diffx / len, diffy / len) --Normalise the distance, this can return type Infinity
        --We don't want this to be infinity^, because in that cause the positions of node1 and node2 are the same, which we will not caluculate

        totalDampingForce = dot normDiff (v1 `sv` v) * damping --Calculate damping
        dampingForce = totalDampingForce * 0.5 --Divide by two to split across both nodes

        forcePerDamped = forcePer `pv` (normDiff `mv` dampingForce)

        --Apply forces and increment velocities
        acceleration = forcePerDamped `dv` m
        acceleration1 = forcePerDamped `dv` (-m1)

        --Update velocities
        vUpdate = v `pv` (acceleration `mv` dt)
        v1Update = v1 `pv` (acceleration1 `mv` dt)

        res = updateNodes nodes [
                (n1, Node m g r c pp p vUpdate),
                (n2, Node m1 g1 r1 c1 pp1 p1 v1Update)
            ]


--Collision functions
--Boundary Collision functions

boundaryCollision :: Node -> Node
boundaryCollision node
    --This code computes if a node is outside of the boundaries, and if so stores the normal off the wall.
    --It also pushes the node inside of the boundary, and then with reflectNormal reflects the velocity off of the wall.
    --The current reflection process may be slightly heavy handed for this process, as the normals can be hardcoded for every situation,
    -- but the difference in computation is completely neglegible.
    --The reflectNormal function also takes care of friction parallel to the slope.

    | collided = reflectNormal newNode normal
    | otherwise = node
    where
        (collided, normal, newNode) = boundaryCheck node

boundaryCheck :: Node -> (Bool, Vec2f, Node)
boundaryCheck (Node m g r c pp (x, y) v)
    --Right wall
    | x > screenX - r = (True, (1.0, 0.0), moveNode currNode (screenX - r - x - epsilon, 0.0))
    --Left wall
    | x < r = (True, (-1.0, 0.0), moveNode currNode (r - x + epsilon, 0.0))
    --Floor
    | y > screenY - r = (True, (0.0, 1.0), moveNode currNode (0.0, screenY - r - y - epsilon))
    --Roof
    | y < r = (True, (0.0, -1.0), moveNode currNode (0.0, r - y + epsilon))
    --Did not collide
    | otherwise = (False, (0.0, 0.0), currNode)

    where
        currNode = Node m g r c pp (x, y) v --Makes definition easier to read
        epsilon = 0.5 --This value is to add stability for node-boundary collisions
        --More specifically it stops a node from getting caught outside of the boundaries due to instability.

reflectNormal :: Node -> Vec2f -> Node
reflectNormal (Node m g r c pp p v) normal = Node m g r c pp p newV
    where
        reflectedV = v `sv` (normal `mv` (2.0 * dot v normal))

        (nx, ny) = normal
        normalGrad = ny / nx
        grad = -1.0 / normalGrad

        orthogSlopeForce = m * g * cos (atan grad) -- R = mgcos(theta) 
        friction = frictionConstant * orthogSlopeForce -- Fr = uR

        newV
            | pythag reflectedV == 0 = v
            | otherwise = reflectedV `sv` ((normalize reflectedV `mv` friction) `dv` m)



--Node to node collision functions

handleCollision :: Node -> Node -> (Node, Node)
handleCollision node1 node2 = (updatedNode1, updatedNode2)
    where
        --Unpack nodes
        (Node m g r colRad pastPos pos vel) = node1
        (Node _ _ _ _ pastPos1 pos1 vel1) = node2

        --Get difference in their positions 
        diff = pos1 `sv` pos
        n = normalize diff --Norm Difference


        --Move each node and store the new position values
        --This is moving each node on their shared axis away from eachover at a magnitude of half of the distance between them
        --Node that one of them has (-c) this is because the differnce calculation is node2 - node1, so node1 must be negative here 
        (Node _ _ _ _ newPP newP _) = moveNode node1 ((n `mv` (-colRad)) `pv` (diff `mv` 0.5))
        (Node _ _ _ _ newPP1 newP1 _) = moveNode node2 ((n `mv` colRad) `sv` (diff `mv` 0.5))

        --This conserves momentum and energy while reflecting the velocities.
        --Without conserving momentum and energy node-node collision creates energy, greatly decreasing stability. 
        conserve = (dot vel n - dot vel1 n) / m
        resultant = n `mv` (conserve * m)

        finalVel = vel `sv` resultant
        finalVel1 = vel1 `pv` resultant
        
        --Update nodes
        updatedNode1 = Node m g r colRad newPP newP finalVel
        updatedNode2 = Node m g r colRad newPP1 newP1 finalVel1

doCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Node -> Node -> Vector (Vector Node)
doCollision nodes i j k p node1 node2 = updated
    where
        (collidedNode1, collidedNode2) = handleCollision node1 node2
        updated = updateNodes nodes [((i, j), collidedNode1), ((k, p), collidedNode2)]


handleOneCol :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Vector (Vector Node)
handleOneCol nodes i j k p
    | (i /= k || j /= p) && collisionPred node1 node2 = doCollision nodes i j k p node1 node2
    | otherwise = nodes 
    where
        node1 = nodes ! i ! j
        node2 = nodes ! k ! p


perNodeRowCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Vector (Vector Node)
perNodeRowCollision nodes i j k p   
    | p <= j + 1 = seq rowFullRes (seq handleOneRes rowFullRes)
    | otherwise = nodes
    where
        handleOneRes
            | not (inBounds k (V.length nodes) && inBounds p (V.length (nodes ! 0))) = nodes
            | otherwise = handleOneCol nodes i j k p

perNodeRowCollisionFull :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Vector (Vector Node)
perNodeRowCollisionFull nodes i j k p   
    | p < V.length (nodes ! k) = seq rowFullRes (seq handleOneRes rowFullRes)
    | otherwise = nodes
    where
        handleOneRes = handleOneCol nodes i j k p
        rowFullRes = perNodeRowCollisionFull handleOneRes i j k (p + 1) 

perNodeCollisionFull :: Vector (Vector Node) -> Int -> Int -> Int -> Vector (Vector Node)
perNodeCollisionFull nodes i j k
    | k < V.length nodes = seq fullRes(seq rowRes fullRes)
    | otherwise = nodes
    where
        rowRes = perNodeRowCollisionFull nodes i j k 0
        fullRes = perNodeCollisionFull rowRes i j (k + 1)

perNodeCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Vector (Vector Node)
perNodeCollision nodes i j k
    | k <= i + 1 = seq fullRes(seq rowRes fullRes)
    | otherwise = nodes
    where
        rowRes = perNodeRowCollision nodes i j k (j - 1)
        fullRes = perNodeCollision rowRes i j (k + 1)
        


collisionPred :: Node -> Node -> Bool
collisionPred node1 node2 = diffL /= 0.0 && diffL < c + c1
    where
        diffL = pythag $ p `sv` p1
        (Node m g r c pp p v) = node1
        (Node m1 g1 r1 c1 pp1 p1 v1) = node2

        


nodeCollisionRow :: Vector (Vector Node) -> Int -> Int -> Vector (Vector Node)
nodeCollisionRow nodes i j
    | j < V.length(nodes ! i) = seq nodeColRowRes (seq colRes nodeColRowRes)
    | otherwise = nodes
    where
        nodeColRowRes = nodeCollisionRow colRes i (j + 1)
        --colRes = perNodeCollision nodes i j (i - 1)
        colRes = perNodeCollisionFull nodes i j 0


nodeCollisions :: Vector (Vector Node) -> Int -> Vector (Vector Node)
nodeCollisions nodes i
    | i >= V.length nodes = nodes
    | otherwise = seq colRes (seq rowRes colRes)
    where
        colRes = nodeCollisions rowRes (i + 1)
        rowRes = nodeCollisionRow nodes i 0


--Functions to initialize the structure and create the first state
createPartition :: Vector (Vector Node) -> Int -> Int -> Float -> Float -> Float -> Vector Spring
createPartition nodes i j d sc al = firstSpring `cons` full 
    where
        firstSpring = Spring d sc al (i, j) (i - 1, j)
        secondSpring = Spring d sc al (i, j) (i, j + 1)
        thirdSpring = Spring d sc diagLength (i, j) (i - 1, j + 1)
        fourthSpring = Spring d sc diagLength (i - 1, j) (i, j + 1)
        
        diagLength = pythag (al, al)
        full
            | j < V.length (nodes ! i) - 1 = fromList [secondSpring, thirdSpring, fourthSpring]
            | otherwise = empty

bottomPartitions :: Vector (Vector Node) -> Vector Spring -> Int -> Float -> Float -> Float -> Vector Spring
bottomPartitions nodes springs ind d sc al
    | ind == V.length nodes = springs
    | otherwise = partitionRow ind 0 V.++ bottomPartitions nodes springs (ind + 1) d sc al

    where
        partitionRow i j
            | j == V.length (nodes ! 0) = empty
            | otherwise = createPartition nodes i j d sc al V.++ partitionRow i (j + 1)

topSprings :: Vector (Vector Node) -> Vector Spring -> Int -> Float -> Float -> Float -> Vector Spring
topSprings nodes springs j d sc al
    | j < V.length (nodes ! 0) - 1 = Spring d sc al (0, j) (0, j + 1) 
                                      `cons`
                                      topSprings nodes springs (j + 1) d sc al 
    | otherwise = empty
        

initializeSpringStructure :: Vector (Vector Node) -> Float -> Float -> Float -> Vector Spring
initializeSpringStructure nodes d sc al = bot V.++ top
    where
        bot = bottomPartitions nodes empty 1 d sc al
        top = topSprings nodes empty 0 d sc al

initializeNodesRow :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> Float -> Float -> (Float, Float) -> Vector Node
initializeNodesRow i j w h m g r c al origin
    | j == w = empty
    | otherwise = Node m g r c pos pos (0.0, 0.0) `cons` initializeNodesRow i (j + 1) w h m g r c al origin
    where
        pos = (fromIntegral j * al, fromIntegral i * al) `pv` origin

initializeNodeStructure :: Int -> Int -> Int -> Float -> Float -> Float -> Float -> Float -> (Float, Float) -> Vector (Vector Node)
initializeNodeStructure i w h m g r c al origin
    | i == h = empty
    | otherwise = initializeNodesRow i 0 w h m g r c al origin `cons` initializeNodeStructure (i + 1) w h m g r c al origin


initial :: Int -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float) -> Simulation
initial nodesW nodesH m g r c d sc al origin = Simulation nodes springs
    where
        nodes = initializeNodeStructure 0 nodesW nodesH m g r c al origin
        springs = initializeSpringStructure nodes d sc al