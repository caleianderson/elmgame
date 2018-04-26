--Code by Cale Anderson for PFP 2018

import Html exposing (Html, div, text)
import Time exposing (Time, second)
import Keyboard exposing (..)
import Char exposing (..)
import List exposing (..)
import String exposing(..)
import Markdown exposing(..)
--import Array2D exposing (..)

--TODOLIST  add enemies, make them move randomly, make shooting kill enemies and make them shoot back,
-- , make map size choosable

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model = {
 playerPos: Int,
 bullets: List(Int)
}

--position currently using a 3x3 grid
initialModel = {
   playerPos = 94,
   bullets = []
  }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- UPDATE
type Msg = Presses Char

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Presses code ->
      case (code, model.playerPos) of
        ('w', x)->
          if (x==0) || (x==1) || (x==2) || (x==3) || (x==4) || (x==5) || (x==6) || (x==7) || (x==8) || (x==9)then
            ({playerPos = x, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
          else
            ({playerPos = x-10, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
        ('a', x)->
          if (x==0) || (x==10) || (x==20) || (x==30) || (x==40)  || (x==50) || (x==60) || (x==70) || (x==80) || (x==90)then
            ({playerPos = x, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
          else
            ({playerPos = x-1, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
        ('s', x)->
          if (x==90) || (x==91) || (x==92) || (x==93) || (x==94)  || (x==95) || (x==96) || (x==97) || (x==98) || (x==99)then
            ({playerPos = x, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
          else
            ({playerPos = x+10, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
        ('d', x)->
          if (x==9) || (x==19) || (x==29) || (x==39) || (x==49) || (x==59) || (x==69) || (x==79) || (x==89) || (x==99) then
            ({playerPos = x, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
          else
            ({playerPos = x+1, bullets = List.map (\ x->x-10) model.bullets},Cmd.none)
        (' ',x)->
            ({playerPos = x, bullets =  List.map (\ x->x-10) (List.append model.bullets [x])},Cmd.none)

        (_,_)-> (model,Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
      Keyboard.presses (\code -> Presses (fromCode code))

generateMap : Int -> List(Int) -> Int -> Int -> String
generateMap player bul tot count=
  case (player, bul, tot, count) of
    (0,_,0,_) -> "⍋ "
    (_,_,0,_) -> "⎕ "

    ---bullets can't be in the final row so the first case isn't nescasary

--old bullets
    (x,0::t,y,9) -> String.append "۞ <br> <br>" (generateMap (x-1) (List.map (\ x->x-1) t) (y-1) 0)
    (x,0::t,y,z) -> String.append "۞ " (generateMap (x-1) (List.map (\ x->x-1) t) (y-1) (z+1))

    (0,_,x,9) -> String.append "⍋ <br> <br>" (generateMap (-1) (List.map (\ x->x-1) bul) (x-1) 0)
    (y,_,x,9) -> String.append "⎕ <br> <br>" (generateMap (y-1) (List.map (\ x->x-1) bul) (x-1) 0)
    (0,_,x,z) -> String.append "⍋ " (generateMap (-1) (List.map (\ x->x-1) bul) (x-1) (count+1))
    (y,_,x,z) -> String.append "⎕ " (generateMap (y-1) (List.map (\ x->x-1) bul) (x-1) (count+1))


--solves the problems of bullets with negative coordinate values
bulletUpdate: List Int -> List Int
bulletUpdate ls =
  case ls of
    [] -> []
    h::t ->
      if (h<0) then
        bulletUpdate t
      else
        ls

iHateMaybes : Maybe Int -> Int
iHateMaybes val =
  case val of
    Just x -> x
    _ -> 0

iHateMaybes2 : Maybe(List(Int)) -> List Int
iHateMaybes2 val =
  case val of
    Just(x) -> x
    _ -> []

-- VIEW
view : Model -> Html Msg
view model =
  let newBul = bulletUpdate model.bullets in
  let myMap =  generateMap model.playerPos newBul 99 0 in
  toHtml [] myMap
