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

        
            

view: Model -> Html Msg
view model =
    Html.div[]
        [svg [width "100%"
             ,height "100%"
             ,viewBox "-400 -400 800 800"
             ]
             ([]++(mazeView model))
        ]
            

mazeView: Model -> List (Html Msg)
mazeView model =
    let
        unit = 50
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
                                    

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none
      
    
          
