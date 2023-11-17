module Main where

import Prelude hiding (replicate, head, tail, map)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.Strict.Vector as V


{-
handleCollision :: (((Int, Int), Node), ((Int, Int), Node)) -> (((Int, Int), Node), ((Int, Int), Node))
handleCollision (((i, j), node1), ((k, p), node2)) = (((i, j), updatedNode1), ((k, p), updatedNode2))
    -- | otherwise = (node1, node2)
    where
        --Unpack nodes
        (Node m g r colRad pastPos pos vel) = node1
        (Node _ _ _ _ pastPos1 pos1 vel1) = node2

        --Get difference in their positions 
        diff = pos1 `sv` pos
        n = normalize diff


        --Move each node and store the new position values
        --This is moving each node on their shared axis away from eachover at a magnitude of half of the distance between them
        --Node that one of them has (-c) this is because the differnce calculation is node2 - node1, so node1 must be negative here 
        (Node _ _ _ _ newPP newP _) = moveNode node1 ((n `mv` (-colRad)) `pv` (diff `mv` 0.5))
        (Node _ _ _ _ newPP1 newP1 _) = moveNode node2 ((n `mv` colRad) `sv` (diff `mv` 0.5))

        --reflectedVel = n `mv` (-1.0 * dot n vel) --Reflected velocity for node1
        --reflectedVel1 = n `mv` (-1.0 * dot n vel1) --Reflected velocity for node2

        conserve = (dot vel n - dot vel1 n) / m
        resultant = n `mv` (conserve * m)

        finalVel = (vel `sv` resultant) `mv` 0.99
        finalVel1 = (vel1 `pv` resultant) `mv` 0.99


        
        --Update nodes
        updatedNode1 = Node m g r colRad newPP newP finalVel
        updatedNode2 = Node m g r colRad newPP1 newP1 finalVel1
        --updatedNode1 = Node m g r colRad newPP newP vel
        --updatedNode2 = Node m g r colRad newPP1 newP1 vel1

type Collisions = Vector (((Int, Int), Node), ((Int, Int), Node))

checkCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Collisions
checkCollision nodes i j k p
    | collisionPred node1 node2 = fromList [(((i, j), node1), ((k, p), node2))]
    | otherwise = empty
    where
        node1 = iNode nodes (i, j)
        node2 = iNode nodes (k, p)

getNodeRowCollisions :: Vector (Vector Node) -> Collisions -> Int -> Int -> Int -> Int -> Collisions
getNodeRowCollisions nodes cols i j k p   
    | p < V.length(nodes ! k) = seq rowFullRes (seq getCol rowFullRes)
    | otherwise = cols
    where
        getCol = checkCollision nodes i j k p
        rowFullRes = getNodeRowCollisions nodes (cols V.++ getCol) i j k (p + 1)

getNodeCollisions :: Vector (Vector Node) -> Collisions -> Int -> Int -> Int -> Collisions
getNodeCollisions nodes cols i j k
    | k < V.length nodes = seq fullRes(seq rowRes fullRes)
    | otherwise = cols
    where
        rowRes = getNodeRowCollisions nodes cols i j k 0
        fullRes = getNodeCollisions nodes (cols V.++ rowRes) i j (k + 1)

concatSplitCols :: Collisions -> Vector ((Int, Int), Node)
concatSplitCols cols
    | cols == empty = empty
    | otherwise = currn1 `cons` fromList [currn2] V.++ concatSplitCols others
    where
        (currn1, currn2) = head cols
        others = tail cols

hasDuplicate :: Collisions -> (((Int, Int), Node), ((Int, Int), Node)) -> Bool
hasDuplicate cols (col1, col2)
    | cols == empty = False
    | otherwise = (col1 == otherCol2 && col2 == otherCol1) || (col1 == otherCol1 && col2 == otherCol2)
                    || hasDuplicate otherCols (col1, col2)
    where 
        (otherCol1, otherCol2) = head cols
        otherCols = tail cols


removeDuplicateCols :: Collisions -> Collisions
removeDuplicateCols cols = V.filter (hasDuplicate cols) cols
-}

type Vec2f = (Float, Float)

pythag :: Vec2f -> Float
pythag (x1, y1) = sqrt (x1 * x1 + y1 * y1)

dot :: Vec2f -> Vec2f -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

normalize :: Vec2f -> Vec2f
normalize (x, y) = (x / l, y / l)
    where l = pythag (x, y)

data Spring = Spring {
    damping :: Float,
    springConstant :: Float,
    anchorLength :: Float,
    node1 :: (Int, Int),
    node2 :: (Int, Int)
} deriving(Show)

data Node = Node {
    mass :: Float,
    gravity :: Float,
    radius :: Float,
    collisionRadius :: Float,
    pastPos :: Vec2f,
    position :: Vec2f,
    velocity :: Vec2f
} deriving(Eq, Show)

pv :: Vec2f -> Vec2f -> Vec2f 
pv (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

mv :: Vec2f -> Float -> Vec2f
mv (x1, y1) f = (x1 * f, y1 * f)

sv :: Vec2f -> Vec2f -> Vec2f
sv (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dv :: Vec2f -> Float -> Vec2f
dv (x, y) f = (x / f, y / f)

moveNode :: Node -> Vec2f -> Node
moveNode (Node m g r c pp p v) diff = Node m g r c p (p `pv` diff) v

addGravity :: Float -> ViewPort -> Node -> Node
addGravity dt view (Node m g r c pp p v) = Node m g r c pp (p `pv` newV) newV
    where
        theta = viewPortRotate view * (pi / 180.0) 
        gravVec = (sin theta, cos theta)
        newV = v `pv` (gravVec `mv` (dt * g))

data Simulation = Simulation {
    nodes :: Vector (Vector Node),
    springs :: Vector Spring
} deriving(Show)

iNode :: Vector (Vector Node) -> (Int, Int) -> Node
iNode nodes (i, j) = nodes ! i ! j

getPos :: Vector (Vector Node) -> (Int, Int) -> Vec2f
getPos nodes ind = pos
    where
        (Node m g r c pp pos v) = iNode nodes ind

i1i2 :: Int -> Int -> (Int, Int) 
i1i2 ind w = (i, j)
    where
        i = ind `div` w 
        j = ind `rem` w

screenX :: Float
screenX = 1050.0

screenY :: Float 
screenY = 900.0

frictionConstant :: Float
frictionConstant = 0.01


updateNodes :: Vector (Vector a) -> [((Int, Int), a)] -> Vector (Vector a) 
updateNodes nodes [] = nodes
updateNodes nodes (((i, j), newNode) : updates) = seq newNodes (updateNodes newNodes updates)
    where
        newNodes = nodes // [(i, (nodes ! i) // [(j, newNode)])]

--Calculates spring force for a single spring
physics :: Vector (Vector Node) -> Spring -> Float -> Vector (Vector Node)
physics nodes (Spring damping springConstant anchorLength n1 n2) dt
    | len == 0 = nodes
    | otherwise = res
    where
        (Node m g r c pp p v) = iNode nodes n1 --Index nodes
        (Node m1 g1 r1 c1 pp1 p1 v1) = iNode nodes n2

        (diffx, diffy) = p1 `sv` p
        theta = atan2 diffy diffx
        len = pythag (diffx, diffy)

        tension = springConstant * (len - anchorLength) --Hooke' law
        forcePer = (tension / 2.0 * cos theta, tension / 2.0 * sin theta)
        --Node 1 has -forcePer acting, and forcePer for Node 2 since diff = node2 - node1

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



springPhysics :: Vector (Vector Node) -> Vector Spring -> Float -> Vector (Vector Node)
springPhysics nodes springs dt 
    | V.null springs = nodes
    | otherwise = seq physicsRes (springPhysics physicsRes other dt) 
    where
        physicsRes = physics nodes spring dt
        spring = head springs
        other = tail springs


physicsStep :: Simulation -> Float -> ViewPort -> Simulation
physicsStep (Simulation nodes springs) dt view = seq hookesRes (Simulation hookesRes springs)
    where
        hookesRes = seq gravRes (springPhysics gravRes springs dt) --Calculating spring forces
        gravRes = (map.map) (addGravity dt view) nodes --Calculating gravity forces



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


boundaryCollision :: Node -> Node
boundaryCollision node
    | collided = reflectNormal newNode normal
    | otherwise = node
    where
        (collided, normal, newNode) = boundaryCheck node





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
{-
recurHandleCollisions :: Node -> Vector (Int, Node) -> (Node, Vector (Int, Node))
recurHandleCollisions n n1L
    | V.length n1L == 0 = (n, n1L)
    | otherwise = seq nextNews (seq newCurr (newCurr, otherNewWithInd `cons` nextNews))
    where
        (_, nextNews) = recurHandleCollisions newCurr n1s
        (newCurr, otherNew) = handleCollision1 n n1

        otherNewWithInd = seq otherNew (ind, otherNew)

        (ind, n1) = head n1L
        n1s = tail n1L

--This function calculates the node collision for a single node by looping another (w*h) times through all the nodes
perNodeCollisionOne :: Vector (Vector Node) -> Int -> Int -> Vector (Vector Node)
perNodeCollisionOne nodes i j = seq updates (updateNodes nodes (toList updates))
    where
        (w, h) = (V.length(nodes ! i), V.length nodes)
        iFlat = V.zip (enumFromN 0 (w*h)) (flatten nodes)--Flatten new nodes and zip them with their indexes(1d)

        currNode = iNode nodes (i, j)

        collisions = seq iFlat (V.filter colPred iFlat) 
        colPred (ind, node) = collisionPred currNode node

        (newCurrNode, otherNodes) = seq collisions (recurHandleCollisions currNode collisions)

        updates = map mapInd2D otherNodes

        mapInd2D (inde, node) = (i1i2 inde w, node)
-}
doCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Node -> Node -> Vector (Vector Node)
doCollision nodes i j k p node1 node2 = updated
    where
        (collidedNode1, collidedNode2) = handleCollision node1 node2
        updated = updateNodes nodes [((i, j), collidedNode1), ((k, p), collidedNode2)]
        --updated = updateNodes nodes []
        --updated = nodes 


handleOneCol :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Vector (Vector Node)
handleOneCol nodes i j k p
    | (i /= k || j /= p) && collisionPred node1 node2 = doCollision nodes i j k p node1 node2
    | otherwise = nodes 
    where
        node1 = iNode nodes (i, j)
        node2 = iNode nodes (k, p)

--Exclusive range bound
inBounds :: Int -> Int -> Bool
inBounds ind bound = ind >= 0 && ind < bound

--Inclusive full bound
fullBound :: Int -> Int -> Int -> Bool
fullBound ind lower upper = ind >= lower && ind <= upper

perNodeRowCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Int -> Vector (Vector Node)
perNodeRowCollision nodes i j k p   
    | p <= j + 1 = seq rowFullRes (seq handleOneRes rowFullRes)
    | otherwise = nodes
    where
        handleOneRes
            | not (inBounds k (V.length nodes) && inBounds p (V.length (nodes ! 0))) = nodes
            | otherwise = handleOneCol nodes i j k p
           -- | otherwise = nodes
        rowFullRes = perNodeRowCollision handleOneRes i j k (p + 1) 

lineCollision :: Node -> (Vec2f, Vec2f) -> Vec2f -> Node
lineCollision (Node m g r c pastPos pos vel) (point1, point2) pushNormal
    | collides = seq moveDistance (Node m g r c pos (pushNormal `mv` moveDistance) reflectedVel)
    | otherwise = Node m g r c pastPos pos vel
    where
        diff = point2 `sv` point1
        toNode = point2 `sv` pos

        --Then project toNode onto diff to get the closest point on the line (with respect to point2)
        closestPoint = diff `mv` (dot toNode diff / dot toNode toNode) 

        --We can then get the difference between the closest point and the original node to check for a collision
        collisionDiff = seq closestPoint (pos `sv` closestPoint)
        collisionL = seq collisionDiff (pythag collisionDiff)
        collides = collisionL <= c

        --This calculates the distance that is needed to be moved, not that this is intentionally not a vector
        --We need to mutliply pushNormal by the move distance, as depending on how far the node is behind the line,
        -- the move vector could send it further in the direction we do not want.
        moveDistance = pythag $! collisionDiff `mv` (c / collisionL - 1.0)

        --Reflect velocity
        reflectedVel = vel `sv` (pushNormal `mv` (dot pushNormal vel * 2.0))





iterateCollisionsJ :: Int -> Vector (Vector Node) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Vector (Vector Node)
iterateCollisionsJ increasing nodes (i, j) (p, k) (pp, pk) --Last values are for the past node
    | fullBound k (j - 1) (j + 1) = if p /= pp && k /= pk then undefined else undefined
    | otherwise = nodes -- End of iteration
    where
        (Node m g r c pastPos pos vel) = nodes ! i ! j
        (Node _ _ _ _ _ posCurr _) = nodes ! p ! k
        (Node _ _ _ _ _ posPast _) = nodes ! pp ! pk

        collisionRes = nodes 
        nextRes = seq collisionRes (iterateCollisionsJ increasing collisionRes (i, j) (p, k + increasing) (p, k))

perNodeCollision :: Vector (Vector Node) -> Int -> Int -> Int -> Vector (Vector Node)
perNodeCollision nodes i j k
    | k <= i + 1 = seq fullRes(seq rowRes fullRes)
    -- | k < V.length nodes = seq fullRes(seq rowRes fullRes)
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
        colRes = perNodeCollision nodes i j (i - 1)

        --collisions = getNodeCollisions nodes empty i j 0 --Get all collisions before handling
        --handledCols = Prelude.foldl (Prelude.++) $ map (splitCollision . toList . handleCollision) collisions --Handle collisions
        --handledCols = concatSplitCols $! map handleCollision collisions
        
        --colRes = seq handledCols (updateNodes nodes (toList handledCols))

nodeCollisions :: Vector (Vector Node) -> Int -> Vector (Vector Node)
nodeCollisions nodes i
    | i >= V.length nodes = nodes
    | otherwise = seq colRes (seq rowRes colRes)
    where
        colRes = nodeCollisions rowRes (i + 1)
        rowRes = nodeCollisionRow nodes i 0

collisionStep :: Simulation -> Float -> Simulation
collisionStep (Simulation nodes springs) dt = seq nodesCollisionRes (Simulation nodesCollisionRes springs)
    where
        nodesCollisionRes = seq boundaryRes (nodeCollisions boundaryRes 0)
        --nodesCollisionRes = boundaryRes
        boundaryRes = (map.map) boundaryCollision nodes


step :: ViewPort -> Float -> Simulation -> Simulation
step view dt sim = collisionRes
    where
        physicsRes = physicsStep sim dt view
        collisionRes = seq physicsRes (collisionStep physicsRes dt)

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

translatePointOrigin :: Vec2f -> Vec2f
translatePointOrigin (x, y) = (x - (screenX / 2.0), (screenY / 2.0) - y)

springToPicture :: Vector (Vector Node) -> Spring -> Picture
springToPicture nodes (Spring d sc al n1 n2) = col line
    where
        diff = getPos nodes n2 `sv` getPos nodes n1
        pushOrPull = (al - pythag diff) * 0.1
        colorCoef = tanh pushOrPull --Put within -1<->1 range
        
        line = Line [translatePointOrigin(getPos nodes n1), translatePointOrigin(getPos nodes n2)]

        --Red = Spring is compressed, blue = spring is extended
        col = color $! makeColor (0.3 + max 0 colorCoef) 0.3 (0.3 + (-1.0) * min 0 colorCoef) 1.0


nodeToPicture :: Node -> Picture
nodeToPicture (Node m g r c pp p v) = trans $ col cir
    where
        cir = Circle r
        col = color $ makeColor 0.4 0.4 0.4 1.0

        (x, y) = translatePointOrigin p
        trans = translate x y


flatten :: Vector (Vector a) -> Vector a
flatten nodes
    | V.null nodes = empty
    | otherwise = nodeH V.++ flatten nodesT
    where
        nodeH = head nodes
        nodesT = tail nodes

drawWalls :: Picture
drawWalls = Pictures [
                left,right,roof,floor 
            ]
    where
        wallCol = color $ makeColor 0.4 0.4 0.4 1.0
        roof = wallCol $ line [translatePointOrigin(0.0, 0.0), translatePointOrigin(screenX, 0.0)]
        floor = translate 0.0 (-screenY) roof
        left = wallCol $ line [translatePointOrigin(0.0, 0.0), translatePointOrigin(0.0, screenY)]
        right = translate screenX 0.0 left
        

draw :: Simulation -> Picture
draw (Simulation nodes springs) = res
    where
        res = Pictures (drawWalls : toList (
                --map nodeToPicture flattened V.++
                map (springToPicture nodes) springs
            ))

        flattened = flatten nodes

main :: IO ()
main = simulate
            (InWindow "SoftHaskell"
                        (floor screenX + 300, floor screenY)
                        (10, 10))
            black
            144
            (initial 14 14 1.0 1.2 9.0 12.0 10.0 15.0 35.0 (150.0, 150.0))
            draw
            step
