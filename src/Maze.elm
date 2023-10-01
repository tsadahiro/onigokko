module Maze exposing (..)

import Browser
import Dict exposing (Dict)
import Random
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

type alias Maze = Dict (Int, Int) (Int, Int)
       
type alias Model = {maze: Maze
                   ,outOfTree: (List (Int, Int))
                   ,currentPos: (Int, Int)
                   ,lerwStart: (Int, Int)
                   }

type Direction = North
               | South
               | East
               | West
    
type Msg = NextGen Direction
       

init: () -> (Model, Cmd Msg)
init _ = ({maze = Dict.empty
          ,outOfTree = vertexList mazeSize
          ,currentPos = (0,0)
          ,lerwStart = (0,0)
          }
         ,Random.generate
             NextGen (nextDir (0,0) mazeSize)
         )

nextDir: (Int, Int) -> Int -> Random.Generator Direction
nextDir (x,y) size =
    let
        east = if x < size then
                   [East]
               else
                   []
        west = if x > (-size) then
                   [West]
               else
                   []
        north = if y < size then
                   [North]
               else
                   []
        south = if y > (-size) then
                   [South]
               else
                   []
        pos = (x,y) 
        dirs = List.concat [east, west, south, north]
    in
        Random.uniform
            (Maybe.withDefault West <| List.head dirs)
            (List.drop 1 dirs)
            

mazeSize = 5
vertexList: Int -> List (Int, Int)
vertexList s =
    List.filter (\(x,y) -> x /= s || y /= s) <|
        List.concat <|
            List.map (\x ->
                          List.map (\y -> (x,y)) (List.range (-s) s)
                     )
                (List.range (-s) s)
    

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NextGen dir ->
            let
                newModel = addToMaze dir model
                --dummy = Debug.log "dir" (dir,newModel.currentPos)
            in
                (newModel
                ,if (List.length newModel.outOfTree) > 0 then
                     Random.generate
                         NextGen (nextDir newModel.currentPos mazeSize)
                 else
                     let
                         dummy = Debug.log "completed" <| model.maze
                     in
                     Cmd.none
                )

addToMaze: Direction -> Model -> Model
addToMaze dir model =
    let
        x = Tuple.first model.currentPos
        y = Tuple.second model.currentPos
        next = case dir of
                   North -> (x, y+1)
                   South -> (x, y-1)
                   East -> (x+1, y)
                   West -> (x-1, y)
        newMaze = Dict.insert model.currentPos next model.maze
                  
        delete: (Int,Int) -> Maze -> List (Int, Int) -> List (Int, Int)
        delete p maze outOfTree =
            if (Tuple.first p) > mazeSize then
                outOfTree
            else if List.member p outOfTree then
                     let
                         dummy = Debug.log "" p
                     in
                         Debug.log "out of tree" <|
                             delete (Maybe.withDefault (mazeSize+1,0) <| Dict.get p newMaze)
                                 newMaze  (List.filter (\q -> p /= q) outOfTree)
                 else
                     outOfTree
        newOutOfTree = if List.member next model.outOfTree then
                           model.outOfTree
                       else -- in tree
                           delete model.lerwStart newMaze model.outOfTree
        newStart = if List.member next model.outOfTree then
                       next
                   else -- in tree
                       --Maybe.withDefault (0,0) <| List.head model.outOfTree
                       Maybe.withDefault (0,0) <| List.head newOutOfTree
                           
        newCurrentPos = if List.member next model.outOfTree then
                            next
                        else -- in tree
                            newStart
    in
        {model |
         maze = newMaze
        ,lerwStart = newStart
        ,outOfTree = newOutOfTree
        ,currentPos = newCurrentPos
        }

        
dual: Maze -> Dict (Int, Int) (List Direction)
dual primal =
    let
        dualV = List.concat <|
                List.map (\x ->
                              List.map (\y -> (x,y)) (List.range (-mazeSize) (mazeSize+1))
                         )
                    (List.range (-mazeSize) (mazeSize+1))
        edges: (Int, Int) -> List Direction
        edges (x,y) =
            if (x,y) == (-mazeSize, -mazeSize) then
                []
            else if x == (-mazeSize) then
                     [South]
                 else if y == (-mazeSize) then
                          [West]
                      else
                          [South, West]
                              
        initialEdges : Dict (Int, Int) (List Direction)
        initialEdges =
            List.foldl (\v dict -> Dict.insert v (edges v) dict ) (Dict.empty) dualV
                
        remove: (Int, Int) -> (Int, Int) -> Dict (Int,Int) (List Direction) -> Dict (Int,Int) (List Direction)
        remove (fromX, fromY) (toX, toY) dict =
            let
                dx = toX-fromX
                dy = toY-fromY
                pdir = if dx > 0 then
                          East
                      else if dx < 0 then
                               West
                           else if dy > 0 then
                                    North
                                else
                                   South
            in
                case pdir of
                    West ->
                        let
                            leftAbove = Maybe.withDefault [] <| Dict.get (fromX, (fromY+1)) dict
                            leftBelow = Maybe.withDefault [] <| Dict.get (fromX, (fromY-1)) dict
                        in
                            Dict.insert (fromX, (fromY-1)) (List.filter (\dir -> dir /= North) leftBelow) <|
                            Dict.insert (fromX, (fromY+1)) (List.filter (\dir -> dir /= South) leftAbove) dict
                    East ->
                        let
                            rightAbove = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY+1)) dict
                            rightBelow = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY-1)) dict
                        in
                            Dict.insert ((fromX+1), (fromY-1)) (List.filter (\dir -> dir /= North) rightBelow) <|
                            Dict.insert ((fromX+1), (fromY+1)) (List.filter (\dir -> dir /= South) rightAbove) dict
                    South ->
                        let
                            leftAbove = Maybe.withDefault [] <| Dict.get ((fromX), (fromY)) dict
                            rightAbove = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY)) dict
                        in
                            Dict.insert ((fromX+1), (fromY)) (List.filter (\dir -> dir /= West) rightAbove) <|
                            Dict.insert ((fromX), (fromY)) (List.filter (\dir -> dir /= East) leftAbove) dict
                    North ->
                        let
                            leftBelow = Maybe.withDefault [] <| Dict.get ((fromX), (fromY+1)) dict
                            rightBelow = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY+1)) dict
                        in
                            Dict.insert ((fromX+1), (fromY+1)) (List.filter (\dir -> dir /= West) rightBelow) <|
                            Dict.insert ((fromX), (fromY+1)) (List.filter (\dir -> dir /= East) leftBelow) dict
                    --_ -> dict
    in
        Dict.foldl (\k v dict -> remove k v dict) initialEdges primal


    

view: Model -> Html Msg
view model =
    Html.div[]
        [svg [width "100%"
             ,height "100%"
             ,viewBox "-300 -300 600 600"
             ]
             ([]++(mazeView model)
                  ++(dualMazeView model)
             )
        ]
            

mazeView: Model -> List (Html Msg)
mazeView model =
    let
        unit = 20
    in
        Dict.foldl
            (\(fromX, fromY) (toX, toY) list ->
                 (line [x1 (String.fromInt (fromX*unit))
                       ,y1 (String.fromInt (fromY*unit))
                       ,x2 (String.fromInt (toX*unit))
                       ,y2 (String.fromInt (toY*unit))
                       ,stroke "black"
                       ,strokeWidth "5"
                       ][]
                 )::list
            )[] model.maze


dualMazeView: Model -> List (Html Msg)
dualMazeView model =
    let
        unit = 20
        paths: (Int, Int) -> List Direction -> List (Html Msg)
        paths (x,y) dirList =
            let
                fromX = (x*unit)-(unit//2)
                fromY = (y*unit)-(unit//2)
            in
            List.map (\dir -> case dir of
                                  East -> line [x1 (String.fromInt fromX)
                                               ,y1 (String.fromInt fromY)
                                               ,x2 (String.fromInt (fromX+unit))
                                               ,y2 (String.fromInt (fromY))
                                               ,stroke "red"
                                               ,strokeWidth "5"
                                               ][]
                                  West -> line [x1 (String.fromInt fromX)
                                               ,y1 (String.fromInt fromY)
                                               ,x2 (String.fromInt (fromX-unit))
                                               ,y2 (String.fromInt fromY)
                                               ,stroke "red"
                                               ,strokeWidth "5"
                                               ][]
                                  North -> line [x1 (String.fromInt fromX)       
                                                ,y1 (String.fromInt fromY)       
                                                ,x2 (String.fromInt fromX)       
                                                ,y2 (String.fromInt (fromY+unit))
                                                ,stroke "red"
                                                ,strokeWidth "5"
                                                ][]
                                  South -> line [x1 (String.fromInt  fromX)       
                                                ,y1 (String.fromInt  fromY)       
                                                ,x2 (String.fromInt  fromX)       
                                                ,y2 (String.fromInt  (fromY-unit))
                                               ,stroke "red"
                                               ,strokeWidth "5"
                                               ][]
                     ) dirList
    in
        Dict.foldl
            (\(x,y) dirs list ->
                 (paths (x,y) dirs)++list
            ) [] (dual model.maze)
            

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none
      
    
          
