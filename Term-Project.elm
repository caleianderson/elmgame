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
 bullets: List(Int),
 enemies: List(Int),
 mode: Int,
 score: Int
}

--position currently using a 3x3 grid
initialModel = {
   playerPos = 94,
   bullets = [],
   enemies = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19],
   mode = 0,
   score = 0
  }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- UPDATE
type Msg = Presses Char

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if (model.enemies == []) then ({playerPos = model.playerPos, bullets = model.bullets, enemies = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19], mode = model.mode, score = model.score},Cmd.none)
  else
  case msg of
    Presses code ->
      let (enemies1 ,newScore) =  remove model.bullets model.enemies ([],model.score) in
      let newEnemies = List.map (\ x->x+1) enemies1 in
      let newBullets = List.map (\ x->x-10) (removeBul model.bullets model.enemies []) in
      case (code, model.playerPos) of
        ('w', x)->
          if (x==0) || (x==1) || (x==2) || (x==3) || (x==4) || (x==5) || (x==6) || (x==7) || (x==8) || (x==9)then
            ({playerPos = x, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
          else
            ({playerPos = x-10, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
        ('a', x)->
          if (x==0) || (x==10) || (x==20) || (x==30) || (x==40)  || (x==50) || (x==60) || (x==70) || (x==80) || (x==90)then
            ({playerPos = x, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
          else
            ({playerPos = x-1, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
        ('s', x)->
          if (x==90) || (x==91) || (x==92) || (x==93) || (x==94)  || (x==95) || (x==96) || (x==97) || (x==98) || (x==99)then
            ({playerPos = x, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
          else
            ({playerPos = x+10, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
        ('d', x)->
          if (x==9) || (x==19) || (x==29) || (x==39) || (x==49) || (x==59) || (x==69) || (x==79) || (x==89) || (x==99) then
            ({playerPos = x, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
          else
            ({playerPos = x+1, bullets = newBullets, enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)
        (' ',x)->
            ({playerPos = x, bullets =  List.append newBullets [x-10], enemies = newEnemies, mode = model.mode, score = newScore},Cmd.none)

        (_,_)-> (model,Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
      Keyboard.presses (\code -> Presses (fromCode code))


--char for enemies or enemy bullets ↡ ࿈ ⇟ ⌔ ⌵ ⍒ ⍢ ⍦ ⑂ ▽
--char for explosion ⎃

--special charecters used ⍋(player), ⎕(blank), ۞(bullet)
generateMap : Int -> List(Int) -> List(Int) -> Int -> Int -> String
generateMap player bul bad tot count=
  case (player, bul, bad, tot, count) of
    (0,_,_,0,_) -> "⍋ "
    (_,_,_,0,_) -> "⎕ "

    (x,h1::t1,h2::t2,y,9) ->
      let test = h1==h2 in
      if (test && (h1==0)) then
        String.append "⎃ <br> <br>" (generateMap (x-1) (List.map (\ x->x-1) t1) (List.map (\ x->x-1) t2) (y-1) 0)
      else if (h1==0) then
        String.append "۞ <br> <br>" (generateMap (x-1) (List.map (\ x->x-1) t1) (List.map (\ x->x-1) bad) (y-1) 0)
      else if (h2==0) then
        String.append "⍒ <br> <br>" (generateMap (x-1) (List.map (\ x->x-1) bul) (List.map (\ x->x-1) t2) (y-1) 0)
      else
        String.append "⎕ <br> <br>" (generateMap (x-1) (List.map (\ x->x-1) bul) (List.map (\ x->x-1) bad) (y-1) 0)

    (x,h1::t1,h2::t2,y,z) ->
        let test = h1==h2 in
        if (test && (h1==0)) then
          String.append "⎃ " (generateMap (x-1) (List.map (\ x->x-1) t1) (List.map (\ x->x-1) t2) (y-1) (z+1))
        else if (h1==0) then
          String.append "۞ " (generateMap (x-1) (List.map (\ x->x-1) t1) (List.map (\ x->x-1) bad) (y-1) (z+1))
        else if (h2==0) then
          String.append "⍒ " (generateMap (x-1) (List.map (\ x->x-1) bul) (List.map (\ x->x-1) t2) (y-1) (z+1))
        else
          String.append "⎕ " (generateMap (x-1) (List.map (\ x->x-1) bul) (List.map (\ x->x-1) bad) (y-1) (z+1))

    (x,[],0::t,y,9) -> String.append "⍒ <br> <br>" (generateMap (x-1) ([]) (List.map (\ x->x-1) t) (y-1) 0)
    (x,[],0::t,y,z) -> String.append "⍒ " (generateMap (x-1) ([]) (List.map (\ x->x-1) t) (y-1) (z+1))

    (x,0::t,en,y,9) -> String.append "۞ <br> <br>" (generateMap (x-1) (List.map (\ x->x-1) t) (List.map (\ x->x-1) en) (y-1) 0)
    (x,0::t,en,y,z) -> String.append "۞ " (generateMap (x-1) (List.map (\ x->x-1) t) (List.map (\ x->x-1) en) (y-1) (z+1))

    (x,[],h::t,y,9) -> String.append "⎕ <br> <br>" (generateMap (x-1) ([]) (List.map (\ x->x-1) bad) (y-1) 0)
    (x,[],h::t,y,z) -> String.append "⎕ " (generateMap (x-1) ([]) (List.map (\ x->x-1) bad) (y-1) (z+1))

    (0,_,b,x,9) -> String.append "⍋ <br> <br>" (generateMap (-1) (List.map (\ x->x-1) bul) (b) (x-1) 0)
    (y,_,b,x,9) -> String.append "⎕ <br> <br>" (generateMap (y-1) (List.map (\ x->x-1) bul) (b) (x-1) 0)
    (0,_,b,x,z) -> String.append "⍋ " (generateMap (-1) (List.map (\ x->x-1) bul) (b) (x-1) (count+1))
    (y,_,b,x,z) -> String.append "⎕ " (generateMap (y-1) (List.map (\ x->x-1) bul) (b) (x-1) (count+1))


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

--returns new list of enemies with the shot ones removed
remove: List Int -> List Int -> (List Int,Int) -> (List Int,Int)
remove bul bad (list,score)=
  case bad of
    [] -> (list,score)
    h::t ->
      if (member h bul) then
        remove bul t (list,score+10)
      else
        remove bul t (list++[h],score)

--returns new list of enemies with the shot ones removed
removeBul: List Int -> List Int -> List Int -> List Int
removeBul bul bad new=
  case bul of
    [] -> new
    h::t ->
      if (member h bad) then
        removeBul t bad new
      else
        removeBul t bad (new++[h])

-- VIEW
view : Model -> Html Msg
view model =
  let newBul = bulletUpdate model.bullets in
  let myMap =  generateMap model.playerPos newBul model.enemies 99 0 in
  let final = String.append myMap (String.append "<br> <br> Score: " (toString model.score)) in
  toHtml [] final
