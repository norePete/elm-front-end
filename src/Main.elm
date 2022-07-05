module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, input, pre, button, div, text)
import Html.Attributes exposing (..)
import Http
import Html.Events exposing (onClick, onInput)


--Main


--declare type
main =
  Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view 
  }




--Model
type alias Content = 
  { content : String
  }

--declare data schema
type Model 
  = Failure
  | Loading
  | Success String
  | Hidden
  | Visible
  | StatusDialogOpen
  | StatusDialogClose
  | UpdateDialogOpen Content
  | UpdateDialogClose 


--declare type
init : () -> (Model, Cmd Msg)
init _=
  ( Loading
  , Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotText
    }
  )


--Update 
type Msg 
  = GotText (Result Http.Error String)
  | Placeholder
  | MakeVisible
  | MakeHidden
  | OpenStatus
  | CloseStatus
  | OpenUpdate
  | CloseUpdate
  | ChangeText String
--declare type
update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
  case msg of 
    GotText result ->
      case result of 
        Ok fullText -> 
          (Success fullText, Cmd.none)
        Err _-> 
          (Failure, Cmd.none)
    Placeholder ->
      (Failure, Cmd.none)
    MakeHidden ->
      (Hidden, Cmd.none)
    MakeVisible ->
      (Visible, Cmd.none)
    OpenStatus ->
      (StatusDialogOpen, Cmd.none)
    CloseStatus -> 
      (StatusDialogClose, Cmd.none)
    OpenUpdate -> 
      (UpdateDialogOpen { content = "" }, Cmd.none)
    CloseUpdate ->
      (UpdateDialogClose, Cmd.none)
    ChangeText newContent ->
      (UpdateDialogOpen { content = newContent}, Cmd.none)

--Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

--View

--declare type
view : Model -> Html Msg

view model =
  case model of 
    Failure -> 
      div []
      [ text "I was unable to load your book."
      , div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] [ button [onClick MakeVisible][text "show"] ]
      , div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]
    Loading -> 
      text "Loading ... "
    Success fullText -> 
      div []
      [ pre [] [text fullText]
      , div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] [ button [onClick MakeVisible][text "show"] ]
      , div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]
    Hidden ->
      div []
      [
       div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] [ button [onClick MakeVisible][text "show"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]
    Visible ->
      div []
      [
       div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]
    StatusDialogOpen -> 
      div []
      [
       div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      ]
    StatusDialogClose -> 
      div []
      [
       div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]
    UpdateDialogOpen userInput-> 
      div []
      [
       div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] 
        [ input 
          [ placeholder "Text to reverse"
          , value userInput.content
          , onInput ChangeText ] [] 
        , div [] [ text (String.reverse userInput.content) ]
        ]
      , div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick CloseUpdate][text "apply"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]
    UpdateDialogClose ->
      div []
      [
       div [] [ button [onClick CloseStatus][text "green"] ]
      , div [] [ button [onClick CloseStatus][text "yellow"] ]
      , div [] [ button [onClick CloseStatus][text "red"] ]
      , div [] [ button [onClick MakeHidden][text "hide"] ]
      , div [] [ button [onClick Placeholder][text "create"] ]
      , div [] [ button [onClick Placeholder][text "delete"] ]
      , div [] [ button [onClick OpenUpdate][text "update"] ]
      , div [] [ button [onClick OpenStatus][text "change status"] ]
      ]

