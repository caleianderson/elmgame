import Html exposing (Html, div, text)
import Time exposing (Time, second)
import Keyboard exposing (..)
import Char exposing (..)
import List exposing (..)
import String exposing(..)
import Markdown exposing(..)
--import Array2D exposing (..)

--TODOLIST add shooting, add enemies, make them move randomly, make shooting kill enemies and make them shoot back,
-- swap to unicode charecters so they are all the same size, make map size choosable





main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model = {
 playerPos: Int
}

--position currently using a 3x3 grid
initialModel = {
   playerPos = 94
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
            (model, Cmd.none)
          else
            ({playerPos = x-10},Cmd.none)
        ('a', x)->
          if (x==0) || (x==10) || (x==20) || (x==30) || (x==40)  || (x==50) || (x==60) || (x==70) || (x==80) || (x==90)then
            (model, Cmd.none)
          else
            ({playerPos = x-1},Cmd.none)
        ('s', x)->
          if (x==90) || (x==91) || (x==92) || (x==93) || (x==94)  || (x==95) || (x==96) || (x==97) || (x==98) || (x==99)then
            (model, Cmd.none)
          else
            ({playerPos = x+10},Cmd.none)
        ('d', x)->
          if (x==9) || (x==19) || (x==29) || (x==39) || (x==49) || (x==59) || (x==69) || (x==79) || (x==89) || (x==99) then
            (model, Cmd.none)
          else
            ({playerPos = x+1},Cmd.none)
        (_,_)-> (model,Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
      Keyboard.presses (\code -> Presses (fromCode code))

generateMap : Int -> Int -> Int -> String
generateMap player tot count=
  case (player, tot, count) of
    (0,0,_) -> "A "
    (_,0,_) -> "O "
    (0,x,9) -> String.append "A <br>" (generateMap (-1) (x-1) 0)
    (y,x,9) -> String.append "O <br>" (generateMap (y-1) (x-1) 0)
    (0,x,z) -> String.append "A " (generateMap (-1) (x-1) (count+1))
    (y,x,z) -> String.append "O " (generateMap (y-1) (x-1) (count+1))

iHateMaybes : Maybe String -> String
iHateMaybes val =
  case val of
    Just x -> x
    _ -> " "


-- VIEW
view : Model -> Html Msg
view model =
  let myMap =  generateMap model.playerPos 99 0 in
  toHtml [] myMap
