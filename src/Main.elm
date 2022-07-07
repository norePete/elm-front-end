module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, value, placeholder, style, type_, checked, name)
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
  , updateBuffer: String
  , changeStatusDialog: Dialog
  , updateList: (List String)
  , urgency: Urgency
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

type Urgency =
    Low
  | Medium
  | High

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
  | UpdateQuote Quote String
  | Clear
  | CreateRequest String
  | MorePlease
  | GotQuote (Result Http.Error RawQuote)
  | Buffer Quote String
  | SubmissionBuffer String
  | SwitchTo Quote Urgency

--declare type
update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
  case msg of 
    CreateRequest currentBuffer ->
      ({model | buffer = "", quotes = 
        QuoteSuccess (List.concat[
          [Quote currentBuffer "created by ..." "me" 2020 Closed "" Closed ["created"] Low]
          , (case model.quotes of 
              QuoteSuccess ql -> ql
              QuoteLoading -> []
              QuoteFailure -> [])
          ])
      }, Cmd.none)
    SubmissionBuffer currentBuffer ->
      ({model | buffer = currentBuffer}, Cmd.none)
    Buffer quote currentBuffer ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                       {x | updateBuffer = currentBuffer }
                     False -> 
                       x
                     ) current) 
               }
              , Cmd.none)
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
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
    UpdateQuote quote pointlessString ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                       resetUrgency <| clearBuffer <| appendUpdate x quote.updateBuffer x.updateList
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
    SwitchTo quote urgencyLevel -> 
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                       {x  | urgency = urgencyLevel}
                     False -> 
                       x
                     ) current) 
               }
              , Cmd.none)
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
          
  
resetUrgency: Quote -> Quote
resetUrgency quote =
  {quote | urgency = Low}

clearBuffer: Quote -> Quote
clearBuffer quote =
  {quote | updateBuffer = ""}

appendUpdate: Quote -> String -> List String -> Quote
appendUpdate quote buffer existingUpdates=
  {quote | updateList = List.concat [existingUpdates,[buffer]]}

wrapQuote : RawQuote -> Quote
wrapQuote raw =
  { quote = raw.quote
  , source = raw.source
  , author = raw.author
  , year = raw.year
  , updateDialog = Closed
  , updateBuffer = ""
  , changeStatusDialog = Closed 
  , updateList = ["created"]
  , urgency = Low
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
  [ submissionForm model.status model.buffer
  , h2 [] [text "Random Quotes"]
  , viewQuote model
  , viewBook model.resource
  ]

submissionForm : Dialog -> String -> Html Msg
submissionForm visibility buffer =
      case visibility of 
        Open t ->
          div []
          [ button [onClick Toggle][text "hide"]
          , div [][viewInput "text" "description of request" buffer SubmissionBuffer
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


urgencyColour : Urgency -> String
urgencyColour urgency = 
  case urgency of 
    Low ->
      "green"
    Medium ->
      "yellow"
    High ->
      "red"


viewList : List Quote -> String -> List (Html Msg)
viewList ql name =
          List.map (
            \x -> 
                div [class "row"]
                [ blockquote [style "background-color" (urgencyColour x.urgency)][text x.quote]
                , div [] (List.map (\y ->
                   blockquote [style "color" "green"][text y]) x.updateList)
                , p [ style "text-align" "right" ]
                  [ text "-- "
                  , cite [] [ text x.source ]
                  , text (" by " ++ x.author ++ " (" 
                  ++ String.fromInt x.year
                  ++ ")")
                  ]
                , button [onClick (ToggleStatusDialog x)][text "change status"]
                , viewDialog x.changeStatusDialog [viewRadio x]
                , button [onClick (ToggleUpdateDialog x)][text "update request"]
                , viewDialog x.updateDialog [viewForm "text" "placeholder" x.updateBuffer (Buffer x) (UpdateQuote x)]
                , button [onClick (CloseRequest x)][text "close request"]
                ]) ql


viewDialog : Dialog -> List (Html msg) -> Html msg 
viewDialog dialog html = 
  case dialog of 
    Closed ->
      div[][]
    Open paragraph ->
      div [] html

viewForm : String -> String -> String -> (String -> Msg) -> (String -> Msg) -> Html Msg
viewForm inputType placeholder value callback onclick =
  div []
  [ viewInput inputType placeholder value callback 
  , button [onClick (onclick "pointlessString")][text "apply"]
  ] 

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
      input [ type_ t, placeholder p, value v, onInput toMsg] []

viewRadio : Quote -> Html Msg
viewRadio quote =
  div []
    [ viewPicker 
        [ ((quote.urgency == Low), "low", (SwitchTo quote) Low)
        , ((quote.urgency == Medium), "Medium", (SwitchTo quote) Medium)
        , ((quote.urgency == High), "high", (SwitchTo quote) High)
        ]
    ]

viewPicker : List (Bool, String, msg) -> Html msg
viewPicker options =
  fieldset [] (List.map radio options)

radio : (Bool, String, msg) -> Html msg
radio (isChecked, choiceName, msg) =
  label []
    [ input 
      [ type_ "radio"
      , name choiceName
      , onClick msg 
      , checked isChecked
      ] []
    , (if isChecked then 
        text choiceName
      else 
        text "")
    ]

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

