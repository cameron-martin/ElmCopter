import Time exposing (Time, fps, inSeconds)
import Signal exposing (sampleOn, (~), (<~), foldp)

import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage, move, moveX, moveY, filled, rect, Form, group)
import Color exposing (black)

import Keyboard
import List exposing (map, head, append, filter, any)
import Random exposing (generate, initialSeed)

type alias CartesianVector a = {x : a, y: a}

type alias Acceleration = CartesianVector Float

vAdd : CartesianVector number -> CartesianVector number -> CartesianVector number
vAdd v1 v2 = {x=(v1.x + v2.x), y=(v1.y + v2.y)}

vMult : number -> CartesianVector number -> CartesianVector number
vMult k v = {x=(k * v.x), y=(k * v.y)}

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f xs = case xs of
                   []      -> []
                   (y::ys) -> if f y then dropWhile f ys
                                     else  y::ys


last : List a -> Maybe a
last xs = case xs of
            [] -> Nothing
            (y::[]) -> Just y
            (y::ys) -> last ys

-- Models

type alias Object a = { a | position: CartesianVector Float, velocity: CartesianVector Float }

type alias Copter = Object {}
type alias Obstacle = Object {opening: Float}

type State = Playing | Crashed

type alias Game = { state: State, copter: Copter, obstacles: List Obstacle, obstacleVelocity: Float, distanceTravelled: Float, seed: Random.Seed }

-- Inputs

type alias Input = {space: Bool, delta: Time}


input : Signal Input
input = sampleOn delta <| Input <~ Keyboard.space ~ delta

-- Constants

(gameWidth, gameHeight) = (500, 500)
(minX, maxX) = (-gameWidth/2, gameWidth/2)
(minY, maxY) = (-gameHeight/2, gameHeight/2)

(copterWidth, copterHeight) = (50, 50)
obstacleWidth = 50

defaultGame = {state=Playing, copter=defaultCopter, obstacles=[], obstacleVelocity=obstacleStartVelocity, distanceTravelled=0, seed=initialSeed 1}

defaultCopter = {position={x=-200, y=200}, velocity=zeroVector}

-- In pixels/s^2
gravity : Acceleration
gravity = {x=0, y=-400}

thrust : Acceleration
thrust = {x=0, y=1000}

obstacleAcceleration = {x=-1, y=0}

obstacleStartVelocity = -100

zeroVector : CartesianVector number
zeroVector = {x=0, y=0}

delta : Signal Time
delta = inSeconds <~ fps 60

minObstacleDistance = 200
maxObstacleDistance = 400
openingSize = 200

obstacleGenerator : Float -> Random.Generator Obstacle
obstacleGenerator v = Random.customGenerator <| \seed -> let (opening, seed') = generate (Random.float 0 1) seed
                                                             (distance, seed'') = generate (Random.float minObstacleDistance maxObstacleDistance) seed'
                                                             obstacle         = {opening=opening, position={x=gameWidth/2 + distance, y=0}, velocity={x=v, y=0}}
                                                         in (obstacle, seed'')

-- Update Logic

shouldSpawnObstacle : List Obstacle -> Bool
shouldSpawnObstacle obstacles = case head obstacles of
                                    Nothing -> True
                                    Just obs -> obs.position.x <= gameWidth/2

cleanObstacleList : List Obstacle -> List Obstacle
cleanObstacleList = filter (\{position} -> position.x + obstacleWidth/2 >= -gameWidth/2)

stepGame : Input -> Game -> Game
stepGame input game = case game.state of
                        Playing -> stepPlayingGame input game
                        Crashed -> stepCrashedGame input game

stepPlayingGame : Input -> Game -> Game
stepPlayingGame input game = let (newObstacles, seed') = generate (addObstacle newObstacleVelocity <| stepObstacles input game.obstacles) game.seed
                                 newCopter             = stepCopter input game.copter
                                 newObstacleVelocity   = game.obstacleVelocity + obstacleAcceleration.x*input.delta
                             in stepDetectCrash <| { game | copter <- newCopter, obstacles <- newObstacles, seed <- seed', obstacleVelocity <- newObstacleVelocity }


stepDetectCrash : Game -> Game
stepDetectCrash game = let hasCrashed = detectCrash game.copter game.obstacles
                           newState   = if hasCrashed then Crashed else game.state
                        in { game | state <- newState }

stepCrashedGame : Input -> Game -> Game
stepCrashedGame input game = game

obstacleDimensions : Float -> (Float, Float)
obstacleDimensions opening = ((gameHeight-openingSize)*opening, (gameHeight-openingSize)*(1-opening))

-- Collision detection

detectCrash : Copter -> List Obstacle -> Bool
detectCrash copter obstacles = copterOut copter || any (detectObstacleCrash copter) obstacles

copterOut : Copter -> Bool
copterOut {position} = position.x - copterWidth/2 < minX || position.x + copterWidth/2 > maxX ||
                       position.y - copterHeight/2 < minY || position.y + copterHeight/2 > maxY

detectObstacleCrash : Copter -> Obstacle -> Bool
detectObstacleCrash copter obstacle = let (length1, length2) = obstacleDimensions obstacle.opening
                                      in copter.position.x + copterWidth/2 >= obstacle.position.x - obstacleWidth/2 &&
                                           copter.position.x - copterWidth/2 <= obstacle.position.x + obstacleWidth/2 &&
                                           (copter.position.y + copterHeight/2 >= maxY - length1 || copter.position.y - copterHeight/2 <= minY + length2)




addObstacle : Float -> List Obstacle -> Random.Generator (List Obstacle)
addObstacle velocity obstacles = Random.customGenerator <| \seed -> if shouldSpawnObstacle obstacles then
                                                                    let (obstacle, seed') = generate (obstacleGenerator velocity) seed
                                                                    in (obstacle::obstacles, seed')
                                                              else (obstacles, seed)

stepCopter : Input -> Copter -> Copter
stepCopter {space, delta} copter = let accel = copterAcceleration space
                                     in accelerateObject accel delta copter

stepObstacles : Input -> List Obstacle -> List Obstacle
stepObstacles {delta} obstacles = cleanObstacleList <| map (accelerateObject obstacleAcceleration delta) obstacles

copterAcceleration : Bool -> Acceleration
copterAcceleration space = if space then gravity `vAdd` thrust else gravity


gameState : Signal Game
gameState = foldp stepGame defaultGame input

-- Object Physics

accelerateObject : Acceleration -> Time -> Object a -> Object a
accelerateObject acceleration delta object = let
                                                newVelocity = object.velocity `vAdd` (delta `vMult` acceleration)
                                             in changeObjectVelocity newVelocity delta object

changeObjectVelocity : CartesianVector Float -> Time -> Object a -> Object a
changeObjectVelocity newVelocity delta object = let newPosition = object.position `vAdd` (delta `vMult` (0.5 `vMult` (object.velocity `vAdd` newVelocity)))
                                                in { object | velocity <- newVelocity, position <- newPosition }

--calculatePosition :

-- Maybe a more performant version of this could be used.
tickObject : Time -> Object a -> Object a
tickObject = accelerateObject zeroVector


-- Render Logic

display : Game -> Element
display {copter, obstacles} = collage gameWidth gameHeight <| [displayCopter copter] `append` map displayObstacle obstacles

displayCopter : Copter -> Form
displayCopter {position, velocity} = move (position.x, position.y) (filled black (rect copterWidth copterHeight))

displayObstacle : Obstacle -> Form
displayObstacle {position, opening} = let (size1, size2) = obstacleDimensions opening
                                      in moveX position.x <| group [
                                                                     moveY ((gameHeight - size1)/2) (filled black <| rect obstacleWidth size1),
                                                                     moveY (-(gameHeight - size2)/2) (filled black <| rect obstacleWidth size2)
                                                                   ]

main = display <~ gameState
