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
  }

type Dialog = 
  Open String 
  | Closed

--declare data schema
type alias Model = 
  { resource: Load
    , name: String
    , password: String
    , passwordAgain: String
    , status: Dialog
    , updateDialog: Dialog
    , changeStatusDialog: Dialog
    , quotes: QuoteState
  }

--declare type
init : () -> (Model, Cmd Msg)
init _=
  ( Model Loading "" "" "" Closed Closed Closed QuoteLoading
  , Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotText
    }
  )


--Update 
type Msg 
  = GotText (Result Http.Error String)
  | Name String
  | Password String
  | PasswordAgain String
  | Toggle 
  | ToggleStatusDialog
  | ToggleUpdateDialog
  | Clear
  | MorePlease
  | GotQuote (Result Http.Error Quote)

--declare type
update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
  case msg of 

    Clear ->
      ({model | name = ""}, getRandomQuote)
    MorePlease ->
      (model, getRandomQuote)
    GotQuote result ->
      case result of 
        Ok quote -> 
          case model.quotes of 
            QuoteSuccess current ->
              ({model | quotes = 
                QuoteSuccess (List.concat [[quote], current])
              }, Cmd.none)

            QuoteLoading ->
              ({model | quotes = 
                QuoteSuccess [quote]
              }, Cmd.none)

            QuoteFailure ->
              ({model | quotes = 
                QuoteSuccess [quote]
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
    Password password -> 
      ({ model | password = password }, Cmd.none) 
    PasswordAgain password -> 
      ({ model | passwordAgain = password }, Cmd.none)
    Toggle ->
      if model.status == Closed  then
        ({model | status = Open "visible html element" }, Cmd.none)
      else 
        ({model | status = Closed }, Cmd.none)
    ToggleStatusDialog ->
      if model.status == Closed  then
        ({model | changeStatusDialog = Open "change status" }, Cmd.none)
      else 
        ({model | changeStatusDialog = Closed }, Cmd.none)
    ToggleUpdateDialog ->
      if model.status == Closed  then
        ({model | updateDialog = Open "add an update" }, Cmd.none)
      else 
        ({model | updateDialog = Closed }, Cmd.none)


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
  , viewInput "password" "Password" model.password Password
  , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
  , viewValidation model
  , viewDialog model.status model.name model.name model.name
  , h2 [] [text "Random Quotes"]
  , viewQuote model
  , viewBook model.resource
  --other components
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
        , viewList quotelist model.name model.changeStatusDialog model.updateDialog])




viewList : List Quote -> String -> Dialog -> Dialog -> List (Html Msg)
viewList ql name changeStatusDialog updateDialog =
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
                , button [onClick ToggleStatusDialog][text "change status"]
                , viewMenuDialog changeStatusDialog name name name
                , button [onClick ToggleUpdateDialog][text "update request"]
                , viewMenuDialog updateDialog name name name
                ]) ql



viewDialog : Dialog -> String -> String -> String -> Html Msg
viewDialog dialog z x c = 
  case dialog of 
    Closed ->
      div [][ button [onClick Toggle] [text "open"] ]
    Open paragraph ->
      div []
      [  div [] [text paragraph ]
      ,  viewForm z x c
      ,  div [][ button [onClick Toggle] [text "close"] ]
      ]

viewMenuDialog : Dialog -> String -> String -> String -> Html Msg
viewMenuDialog dialog z x c = 
  case dialog of 
    Closed ->
      div[][]
    Open paragraph ->
      div []
      [  div [] [text paragraph ]
      ,  viewForm z x c
      ]

viewForm : String -> String -> String -> Html Msg
viewForm req desc author =
  div []
  [ viewInput "text" "request" req Name 
  , viewInput "text" "description" desc Name
  , viewInput "text" "made by..." author Name
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


viewValidation : Model -> Html msg
viewValidation model = 
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else 
    div [ style "color" "red" ] [ text "Passwords do not match!" ]


-- HTTP

getRandomQuote : Cmd Msg
getRandomQuote = 
  Http.get
  { url = "https://elm-lang.org/api/random-quotes"
  , expect = Http.expectJson GotQuote quoteDecoder
  }

quoteDecoder : Decoder Quote
quoteDecoder = 
  map4 Quote
    (field "quote" string)
    (field "source" string)
    (field "author" string)
    (field "year" int)

