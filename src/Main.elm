module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, value, placeholder, style, type_)
import Http
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, map4, field, int, string)


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
type Load = 
  Failure
  | Loading
  | Success String 

type QuoteState =
  QuoteFailure
  | QuoteLoading 
  | QuoteSuccess (List Quote)

--Move the status dialog and update dialog
-- into the quote 
type alias Quote = 
  { quote : String 
  , source : String 
  , author : String 
  , year : Int 
  , updateDialog: Dialog
  , changeStatusDialog: Dialog
  }

type alias RawQuote = 
  { quote : String 
  , source : String 
  , author : String 
  , year : Int 
  }

type Dialog = 
  Open String 
  | Closed

--declare data schema
type alias Model = 
  { resource: Load
    , name: String
    , status: Dialog
    , quotes: QuoteState
    , buffer: String
  }

--declare type
init : () -> (Model, Cmd Msg)
init _=
  ( Model Loading "" Closed QuoteLoading ""
  , Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotText
    }
  )


--Update 
type Msg 
  = GotText (Result Http.Error String)
  | Name String
  | Toggle 
  | ToggleStatusDialog Quote
  | ToggleUpdateDialog Quote
  | CloseRequest Quote
  | Clear
  | CreateRequest String
  | MorePlease
  | GotQuote (Result Http.Error RawQuote)
  | Buffer String

--declare type
update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
  case msg of 
    CreateRequest currentBuffer ->
      ({model | buffer = "", quotes = 
        QuoteSuccess (List.concat[
          [Quote currentBuffer "created by ..." "me" 2020 Closed Closed]
          , (case model.quotes of 
              QuoteSuccess ql -> ql
              QuoteLoading -> []
              QuoteFailure -> [])
          ])
      }, Cmd.none)
    Buffer currentBuffer ->
      ({model | buffer = currentBuffer}, Cmd.none)
    Clear ->
      ({model | name = ""}, getRandomQuote)
    MorePlease ->
      (model, getRandomQuote)
    GotQuote result ->
      case result of 
        Ok rawquote -> 
          case model.quotes of 
            QuoteSuccess current ->
              ({model | quotes = 
                QuoteSuccess (List.concat [[wrapQuote rawquote], current])
              }, Cmd.none)
            QuoteLoading ->
              ({model | quotes = 
                QuoteSuccess [wrapQuote rawquote]
              }, Cmd.none)

            QuoteFailure ->
              ({model | quotes = 
                QuoteSuccess [wrapQuote rawquote]
              }, Cmd.none)
        Err _ -> 
          ({model | quotes = QuoteFailure}, Cmd.none)
    GotText result ->
      case result of 
        Ok fullText -> 
          ({model | resource = Success fullText}, Cmd.none)
        Err _-> 
          ({model | resource = Failure}, Cmd.none)
    Name name ->
      ({ model | name = name }, Cmd.none)
    Toggle ->
      if model.status == Closed  then
        ({model | status = Open "visible html element" }, Cmd.none)
      else 
        ({model | status = Closed }, Cmd.none)
    ToggleStatusDialog quote ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                       mutateStatusDialog x
                     False -> 
                       x
                     ) current)
               }
              , Cmd.none)
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
    ToggleUpdateDialog quote ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                       mutateUpdateDialog x
                     False -> 
                       x
                     ) current) 
               }
              , Cmd.none)
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
    CloseRequest quote ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess (List.filter (\x -> x /= quote) current) }
              , Cmd.none)
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
  


wrapQuote : RawQuote -> Quote
wrapQuote raw =
  { quote = raw.quote
  , source = raw.source
  , author = raw.author
  , year = raw.year
  , updateDialog = Closed
  , changeStatusDialog = Closed 
  }

mutateStatusDialog: Quote -> Quote
mutateStatusDialog quote =
  {quote | changeStatusDialog = mutateDialog quote.changeStatusDialog}

mutateUpdateDialog: Quote -> Quote
mutateUpdateDialog quote = 
  {quote | updateDialog = mutateDialog quote.updateDialog}

mutateDialog: Dialog -> Dialog
mutateDialog dialog =
  if dialog == Closed then
    Open "dialog"
  else
    Closed

--Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

--View

--declare type
view : Model -> Html Msg
view model =
  div []
  [ viewInput "text" "Name" model.name Name
  , submissionForm model.status model.buffer
  , h2 [] [text "Random Quotes"]
  , viewQuote model
  , viewBook model.resource
  --other components
  ]

submissionForm : Dialog -> String -> Html Msg
submissionForm visibility buffer =
      case visibility of 
        Open t ->
          div []
          [ button [onClick Toggle][text "hide"]
          , div [][viewInput "text" "description of request" buffer Buffer
                  , button [onClick (CreateRequest buffer)][text "create"]
                  ]
          ] 
        Closed -> 
          div []
          [ button [onClick Toggle][text "show"]
          , div [][]
          ] 

viewQuote : Model -> Html Msg
viewQuote model = 
  case model.quotes of 
    QuoteFailure ->
      div []
      [ text "I could not load a random quote for some reason."
      , button [ onClick MorePlease ] [ text "Try Again!" ]
      ]
    QuoteLoading -> 
      div []
      [ button [ onClick MorePlease ] [ text "get a quote!" ]
      , blockquote [][ text "Loading ..." ]
      , p [ style "text-align" "right" ]
        [ text "-- "
        , cite [] [ text "...." ]
        , text ("....")
        ]
      ]
    QuoteSuccess quotelist ->
      div []
      (List.concat[ 
        [ button [onClick MorePlease][text "Another!"] ]
        , viewList quotelist model.name  ])




viewList : List Quote -> String -> List (Html Msg)
viewList ql name =
          List.map (
            \x -> 
                div [class "row"]
                [ blockquote [][ text x.quote ]
                , p [ style "text-align" "right" ]
                  [ text "-- "
                  , cite [] [ text x.source ]
                  , text (" by " ++ x.author ++ " (" 
                  ++ String.fromInt x.year
                  ++ ")")
                  ]
                , button [onClick (ToggleStatusDialog x)][text "change status"]
                , viewDialog x.changeStatusDialog "radio" name name name
                , button [onClick (ToggleUpdateDialog x)][text "update request"]
                , viewDialog x.updateDialog "text" name name name
                , button [onClick (CloseRequest x)][text "close request"]
                ]) ql



viewDialog : Dialog -> String -> String -> String -> String -> Html Msg
viewDialog dialog t z x c = 
  case dialog of 
    Closed ->
      div [][]
    Open paragraph ->
      div []
      [  div [] [text paragraph ]
      ,  viewForm t z x c
      ]

viewForm : String -> String -> String -> String -> Html Msg
viewForm inputType req desc author =
  div []
  [ viewInput inputType "request" req Name 
  , viewInput inputType "description" desc Name
  , viewInput inputType "made by..." author Name
  , button [onClick Clear][text "create"]
  ] 

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewBook : Load -> Html msg
viewBook state =
  case state of 
    Failure -> 
      div [] [ text "Could not load state"]
    Loading -> 
      div [] [ text "Loading ... "]
    Success fullText -> 
      div [] [ pre [] [text fullText]]




-- HTTP

getRandomQuote : Cmd Msg
getRandomQuote = 
  Http.get
  { url = "https://elm-lang.org/api/random-quotes"
  , expect = Http.expectJson GotQuote quoteDecoder
  }

quoteDecoder : Decoder RawQuote
quoteDecoder = 
  map4 RawQuote
    (field "quote" string)
    (field "source" string)
    (field "author" string)
    (field "year" int)

