port module Hands exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Duration exposing (Duration)
import Svg
import Svg.Attributes as Attr
import Svg.Events as Evt


type Direction = Left
               | Right
               | Other
               | Forward
               | Backward

main = Browser.element {init = init
                       ,update = update
                       ,subscriptions = subscriptions
                       ,view = view
                       }

type alias Model = {handsData: List {x:Float, y:Float, z:Float}
                   ,elapsed: Int
                   }
type Msg = LocateHands Duration
         | Hands (List {x:Float, y:Float, z:Float})
       
port handsReceiver : (List {x:Float, y:Float, z:Float} -> msg) -> Sub msg
       
init : () -> (Model, Cmd Msg)
init _ = (Model [] 0, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hands handsData ->
            ({model | handsData = handsData
             }
            ,Cmd.none)
        LocateHands t ->
            let
                dummy = readHands model
            in
            ({model |elapsed = modBy 5 (model.elapsed+1)
             }
            ,Cmd.none)

readHands: Model -> List Float
readHands model =
            if model.elapsed /= 0 then
                []
            else
                let
                    getPosition idx = Maybe.withDefault {x=0,y=0,z=0}<|
                                  List.head <| List.drop idx model.handsData
                    finger idx = List.take 4 <| (if idx <5 then
                                                     List.drop (1+idx*4) model.handsData
                                                 else
                                                     List.drop (2+idx*4) model.handsData
                                                )
                    oyaAngles = List.map fingerAngle <| List.map (\fidx -> finger fidx )[0,5]
                    maxOyaAngle = Debug.log "oya" <| Maybe.withDefault 0 <| List.maximum oyaAngles
                    dummy = Debug.log "dir" <| handsDirection model.handsData
                    dummy2 = Debug.log "forward" <| goForward model.handsData
                in
                    oyaAngles


pinto : List {x:Float, y:Float, z:Float} -> Bool
pinto handsData =
    let
        getPosition idx = Maybe.withDefault {x=0,y=0,z=0}<|
                          List.head <| List.drop idx handsData
        finger idx = List.take 4 <| (if idx <5 then
                                         List.drop (1+idx*4) handsData
                                     else
                                         List.drop (2+idx*4) handsData
                                    )
        angles = List.map fingerAngle <| List.map (\fidx -> finger fidx )[1,2,3,4,6,7,8,9]
        maxAngle =  Maybe.withDefault (pi/2) <| List.maximum angles
    in
        ((List.length handsData)==42) && (maxAngle < pi/7) -- 4*number of fingers + 1*number of arms

handsDirection : List {x:Float, y:Float, z:Float} -> Maybe Direction
handsDirection handsData =
    let
        getPosition idx = Maybe.withDefault {x=0,y=0,z=0}<|
                          List.head <| List.drop idx handsData
        finger idx = List.take 4 <| (if idx <5 then
                                         List.drop (1+idx*4) handsData
                                     else
                                         List.drop (2+idx*4) handsData
                                    )
        fVectors1 = List.map fingerVector <| List.map (\fidx -> finger fidx )[1,2,3,4]
        fVectors2 = List.map fingerVector <| List.map (\fidx -> finger fidx )[6,7,8,9]
        meanAngle1 = (List.sum <| List.map (\fv -> angle {x=1,y=0,z=0} fv) fVectors1)/4
        meanAngle2 = (List.sum <| List.map (\fv -> angle {x=1,y=0,z=0} fv) fVectors2)/4
    in
        if not (pinto handsData) then
            Nothing
        else
            if (Basics.min meanAngle1 meanAngle2) < pi/4 &&
                (abs (meanAngle1 - meanAngle2)) > pi/4 then
                Just Right
            else
                if Debug.log "max" (Basics.max meanAngle1 meanAngle2) > (3*pi/4) &&
                    (abs (meanAngle1 - meanAngle2)) > pi/4 then
                    Just Left
                else
                    Nothing

goForward : List {x:Float, y:Float, z:Float} -> Maybe Direction
goForward handsData =
    let
        getPosition idx = Maybe.withDefault {x=0,y=0,z=0}<|
                          List.head <| List.drop idx handsData
        finger idx = List.take 4 <| (if idx <5 then
                                         List.drop (1+idx*4) handsData
                                     else
                                         List.drop (2+idx*4) handsData
                                    )
        oyaAngles = List.map fingerAngle <| List.map (\fidx -> finger fidx )[0,5]
        minOyaAngle = Debug.log "oya" <| Maybe.withDefault 0 <| List.minimum oyaAngles
    in
        if minOyaAngle > (3/5*pi) then
            Just Forward
        else
            Nothing

            
norm : {x:Float, y:Float, z:Float} -> Float
norm vec =
    sqrt <| vec.x^2 + vec.y^2 + vec.z^2

innerProd : {x:Float, y:Float, z:Float} -> {x:Float, y:Float, z:Float} -> Float
innerProd v w =
    v.x * w.x + v.y * w.y + v.z * w.z

angle : {x:Float, y:Float, z:Float} -> {x:Float, y:Float, z:Float} -> Float
angle v w =
     (innerProd v w) / ((norm v)*norm(w)) |>  acos

fingerVector: List {x:Float, y:Float, z:Float} -> {x:Float, y:Float, z:Float} 
fingerVector finger =
    let
        first = Maybe.withDefault {x=0,y=0,z=0} <|
                List.head finger
        second = Maybe.withDefault {x=0,y=0,z=0} <|
                List.head <| List.drop 1 <| finger
        third = Maybe.withDefault {x=0,y=0,z=0} <|
                List.head <| List.drop 2 <| finger
        fourth = Maybe.withDefault {x=0,y=0,z=0} <|
                 List.head <| List.drop 3 <| finger
        vec = {x=100*(first.x-fourth.x)
              ,y=100*(first.y-fourth.y)
              ,z=100*(first.z-fourth.z)
              }
    in
        vec
    
              
fingerAngle : List {x:Float, y:Float, z:Float} -> Float
fingerAngle finger =
    let
        first = Maybe.withDefault {x=0,y=0,z=0} <|
                List.head finger
        second = Maybe.withDefault {x=0,y=0,z=0} <|
                List.head <| List.drop 1 <| finger
        third = Maybe.withDefault {x=0,y=0,z=0} <|
                List.head <| List.drop 2 <| finger
        fourth = Maybe.withDefault {x=0,y=0,z=0} <|
                 List.head <| List.drop 3 <| finger
        top = {x=first.x-second.x
              ,y=first.y-second.y
              ,z=first.z-second.z
              }
        bot = {x=third.x-fourth.x
              ,y=third.y-fourth.y
              ,z=third.z-fourth.z
              }
    in
        acos <| (innerProd top bot)/((norm top)*(norm bot))

dist3d : {x:Float, y:Float, z:Float} -> {x:Float, y:Float, z:Float} -> Float
dist3d p q =
    sqrt((p.x-q.x)^2 + (p.y-q.y)^2 + (p.z-q.z)^2)
                        

view: Model -> Html Msg
view model =
    div [][]

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [handsReceiver Hands
              ,Browser.Events.onAnimationFrameDelta
                   (Duration.seconds >> LocateHands)
              ]

