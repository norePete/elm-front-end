module Main exposing (..)
import Date exposing (Date, Interval(..), Unit(..))
import Task exposing (Task)
import Time exposing (Month(..))
import Random
import Browser
import Tuple exposing (first, second)
import Html exposing (..)
import Html.Attributes exposing (classList, class, value, placeholder, id, style, type_, checked, name)
import Http
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string, field, map4, map2, index)
import Json.Decode.Pipeline as JP exposing (required, optional, hardcoded)
import Json.Encode as Encode


--Main


--declare type
main =
  Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view 
  }


type QuoteState =
  QuoteFailure
  | QuoteLoading 
  | QuoteSuccess (List Quote)

type alias ID = 
  { id: Int }


--Move the status dialog and update dialog
-- into the quote 
type alias Quote = 
  { quote: String 
  , source: String 
  , author: String
  , year: String
  , updateDialog: Dialog
  , updateBuffer: String
  , changeStatusDialog: Dialog
  , updateList: (List (String, String))
  , urgency: Urgency
  , id: ID
  , startTime: String
  , endTime: String
  , equipment: String
  , location: String
  , partsUsed: String
  , materialsUsed: String
  , furtherAction: String
  }

type Dialog = 
  Open String 
  | Closed

type Urgency =
    Low
  | Medium
  | High
  | ClosedOut

--declare data schema
type alias Model = 
  { 
    --resource: Load
      name: String
    , status: Dialog
    , quotes: QuoteState
    , buffer: String
    , deadbool: Int
    , startTime: String
    , endTime: String
    , equipment: String
    , location: String
    , partsUsed: String
    , materialsUsed: String
    , furtherAction: String
  }

--declare type
init : () -> (Model, Cmd Msg)
--init _=
 -- (Model "" Closed QuoteLoading "", Cmd.none)
init _=
  (Model "" Closed QuoteLoading "" 1 "" "" "" "" "" "" ""
  , Http.get
    { url = "http://192.168.1.252/server/active"
    , expect = Http.expectJson DataReceived decodeListQuote
    }
  )

--Update 
type Msg 
  = 
  Name String
  --| GotText (Result Http.Error String)
  | Toggle 
  | ToggleDead
  | ToggleStatusDialog Quote
  | ToggleUpdateDialog Quote
  | CloseRequest Quote
  | UpdateQuote Quote String
  | CreateRequest String 
  | Sent (Result Http.Error ())
  | DataReceived (Result Http.Error (List Quote))
  --| MorePlease
  --| GotQuote (Result Http.Error RawQuote)
  | Buffer Quote String
  | SubmissionBuffer String
  | StartTimeBuffer String
  | EndTimeBuffer String
  | EquipmentBuffer String
  | LocationBuffer String
  | PartsUsedBuffer String
  | MaterialsUsedBuffer String
  | FurtherActionBuffer String
  | SwitchTo Quote Urgency
  | NewNumber String Date Int
  | GetDate String Date
  | GetUpdateDate Quote Date
  | GetStatusUpdate Quote String Urgency Date

--declare type
update : Msg -> Model -> (Model, Cmd Msg)

update msg model = 
  case msg of 
    CreateRequest currentBuffer ->
      (model, (generateDate currentBuffer))
    GetDate buff currentDate ->
      (model, (newId buff currentDate))
    NewNumber buff currentDate id->
      ({model | name = "", buffer = "", startTime = "", endTime = "", equipment = "", location = "", partsUsed = "", materialsUsed = "", furtherAction = "", quotes = 
        QuoteSuccess (List.concat[
          [requestFactory (Date.toIsoString currentDate) buff model.name id model.startTime model.endTime model.equipment model.location model.partsUsed model.materialsUsed model.furtherAction]
          , (case model.quotes of 
              QuoteSuccess ql -> ql
              QuoteLoading -> []
              QuoteFailure -> [])
          ])
      }, postNewRequest (requestFactory (Date.toIsoString currentDate) buff model.name id model.startTime model.endTime model.equipment model.location model.partsUsed model.materialsUsed model.furtherAction))
    SubmissionBuffer currentBuffer ->
      ({model | buffer = currentBuffer}, Cmd.none)
    Name name ->
      ({model | name = name}, Cmd.none)
    StartTimeBuffer startTime -> 
      ({model | startTime = startTime}, Cmd.none)
    EndTimeBuffer endTime -> 
      ({model | endTime = endTime}, Cmd.none)
    EquipmentBuffer equipment -> 
      ({model | equipment = equipment}, Cmd.none)
    LocationBuffer location -> 
      ({model | location = location}, Cmd.none)
    PartsUsedBuffer partsUsed -> 
      ({model | partsUsed = partsUsed}, Cmd.none)
    MaterialsUsedBuffer materialsUsed -> 
      ({model | materialsUsed = materialsUsed}, Cmd.none)
    FurtherActionBuffer furtherAction -> 
      ({model | furtherAction = furtherAction}, Cmd.none)
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
    Toggle ->
      if model.status == Closed  then
        ({model | status = Open "visible html element" }, Cmd.none)
      else 
        ({model | status = Closed }, Cmd.none)
    ToggleDead ->
      if model.deadbool == 1 then
        ({model | deadbool = 0}, Http.get
        { url = "http://192.168.1.252/server/inactive"
        , expect = Http.expectJson DataReceived decodeListQuote
        })
      else 
        ({model | deadbool = 1}, Http.get
        { url = "http://192.168.1.252/server/active"
        , expect = Http.expectJson DataReceived decodeListQuote
        })
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
      (model, (generateUpdateDate quote))
    GetUpdateDate quote currentDate ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                        mutateUpdateDialog 
                        --<| resetUrgency 
                        <| clearBuffer 
                        <| appendUpdate x quote.updateBuffer 
                        <| (Date.toIsoString currentDate)
                     False -> 
                       x
                     ) current) 
               }
              , postUpdate 
             -- <| resetUrgency 
              <| appendUpdate quote quote.updateBuffer 
              <| (Date.toIsoString currentDate) )
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
    CloseRequest quote ->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess (List.filter (\x -> x /= quote) current) }
              , postCloseRequest {quote | urgency = ClosedOut})
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)
    Sent result ->
      case result of 
        Result.Ok ok ->
          (model, Cmd.none)
        Result.Err notOk ->
          case notOk of
            Http.BadUrl err ->
              ( model, Cmd.none)
            Http.Timeout ->
              (model, Cmd.none)
            Http.NetworkError ->
              (model, Cmd.none)
            Http.BadStatus code ->
              (model, Cmd.none)
            Http.BadBody code ->
              (model, Cmd.none)
    DataReceived result ->
      case result of 
        Ok data ->
          ({ model | quotes = (QuoteSuccess data)}, Cmd.none)
        Err httpError -> 
          ({ model | quotes = (QuoteSuccess [Quote "ERROR" "" "" "NOW" Closed "" Closed [] High (ID 1000) "" "" "" "" "" "" ""])}, Cmd.none)

    SwitchTo quote urgencyLevel -> 
      (model, (generateStatusUpdate quote urgencyLevel))

    GetStatusUpdate quote statusText urgencyLevel currentDate->
          case model.quotes of 
            QuoteSuccess current ->
              ({ model | quotes = 
               QuoteSuccess 
                 (List.map (\x -> 
                   case x == quote of 
                     True ->
                       appendUpdate {x  | urgency = urgencyLevel} statusText (Date.toIsoString currentDate)
                     False -> 
                       x
                     ) current) 
               }
              , postUpdate <| appendUpdate {quote | urgency = urgencyLevel} statusText (Date.toIsoString currentDate))
            QuoteFailure ->
              (model, Cmd.none)
            QuoteLoading ->
              (model, Cmd.none)

requestFactory: String -> String -> String -> Int -> String -> String -> String -> String -> String -> String -> String -> Quote
requestFactory currentDate buffer name id startTime endTime equipment location partsUsed materialsUsed furtherAction =
  Quote 
  buffer 
  "created..." 
  name 
  currentDate
  Closed 
  "" 
  Closed 
  [("created", currentDate)] 
  Low 
  (ID id)
  startTime 
  endTime 
  equipment 
  location 
  partsUsed 
  materialsUsed 
  furtherAction 

randomNumber: Random.Generator Int
randomNumber = 
  Random.int 1 9999

newId:String -> Date -> Cmd Msg
newId buffer currentDate= 
  Random.generate (NewNumber buffer currentDate) randomNumber


generateDate:String -> Cmd Msg
generateDate buffer = 
  (Date.today |> Task.perform (GetDate buffer))

generateUpdateDate:Quote -> Cmd Msg
generateUpdateDate quote = 
  (Date.today |> Task.perform (GetUpdateDate quote))

generateStatusUpdate: Quote -> Urgency -> Cmd Msg
generateStatusUpdate quote urgencyLevel =
  case urgencyLevel of
    Low -> 
      (Date.today |> Task.perform (GetStatusUpdate quote "status changed to 'LOW'" urgencyLevel))
    Medium -> 
      (Date.today |> Task.perform (GetStatusUpdate quote "status changed to 'MEDIUM'" urgencyLevel))
    High -> 
      (Date.today |> Task.perform (GetStatusUpdate quote "status changed to 'HIGH'" urgencyLevel))
    _ -> Cmd.none


resetUrgency: Quote -> Quote
resetUrgency quote =
  {quote | urgency = Low}

clearBuffer: Quote -> Quote
clearBuffer quote =
  {quote | updateBuffer = ""}

appendUpdate: Quote -> String -> String -> Quote
appendUpdate quote buffer date=
  {quote | updateList = List.concat [quote.updateList ,[(buffer, date)]]}
--appendUpdate: Quote -> String -> List (String, String) -> String -> Quote
--appendUpdate quote buffer existingUpdates date=
--  {quote | updateList = List.concat [existingUpdates,[(buffer, date)]]}

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
      div [][ 
        div []
        [ submissionForm 
        model.status model.buffer model.name model.startTime model.endTime 
        model.equipment model.location model.partsUsed model.materialsUsed 
        model.furtherAction]]
  

submissionForm : Dialog -> String -> String -> String -> String -> String -> String -> String -> String -> String -> Html Msg
submissionForm visibility buffer name startTime endTime equipment location partsUsed materials furtherAction =
      case visibility of 
        Open t ->
          div [class "popup"]
          [ div [class "container"]
            [ div [class "hide_button"][button [onClick Toggle][text "hide"]]
            , div [class "row_1"][ 
                      viewInput "text" "name" name Name
                    , viewInput "text" "start time" startTime StartTimeBuffer
                    , viewInput "text" "end time" endTime EndTimeBuffer
                    ]
            , div [class "row_2"][
                      viewInput "text" "equipment" equipment EquipmentBuffer
                    , viewInput "text" "location (department)" location LocationBuffer
                    ]
            , div [class "row_3"][
                      viewTextArea "job description" buffer SubmissionBuffer
                    ]
            , div [class "row_4"][
                      viewTextArea "parts used" partsUsed PartsUsedBuffer
                    , viewTextArea "materials used" materials MaterialsUsedBuffer
                    ]
            , div [class "row_5"][
                      viewTextArea "further action required" furtherAction FurtherActionBuffer
                    ]
             , div [class "row_6"][button [onClick (CreateRequest buffer)][text "create"]]
            ] 
          ]
        Closed -> 
          div [class "menu_button"][button [onClick Toggle][text "menu"]] 

viewTextArea : String -> String -> (String -> msg) -> Html msg
viewTextArea p v toMsg = 
      textarea [ placeholder p, value v, onInput toMsg] []

viewQuote : Model -> Html Msg
viewQuote model = 
  case model.quotes of 
    QuoteFailure ->
          div [
          ][blockquote [][text "Failed to reach database"]]
    QuoteLoading -> 
          div [
          ][blockquote [][text "Loading ..." ]]
    QuoteSuccess quotelist ->
          div [
          ](viewList quotelist)


urgencyColour : Urgency -> String
urgencyColour urgency = 
  case urgency of 
    Low ->
      "rgba(2,247,23,0.5)"
    Medium ->
      "yellow"
    High ->
      "rgba(243,2,23,0.5)"
    ClosedOut ->
      "rgba(0,0,0,0.4)"


viewList : List Quote -> List (Html Msg)
viewList ql =
          List.map (
            \x -> 
                div []
                [ 
                  viewDialog x.updateDialog [div[class "update-dialog"][viewUpdateForm "What's the update on this request?" x.updateBuffer (Buffer x) (UpdateQuote x)]]
                  [ div [][ button [id "update-button", onClick (ToggleUpdateDialog x)][]
                  , button [id "close-button", onClick (CloseRequest x)][]
                  , viewRadio x]
                  ]
                  , div [] [
                    div []
                    [ 
                      div [][ blockquote [style "font-size" "16px", style "width" "80%"][text "Engineer:   ", text  x.author] ]
                    , div [][ 
                        blockquote [style "font-size" "16px", style "width" "80%"][text "Start Time:   ", text x.startTime] 
                      , blockquote [style "font-size" "16px", style "width" "80%"][text "End Time:   ", text x.endTime]]
                    , div [][ 
                        blockquote [style "font-size" "16px", style "width" "80%"][text "Equipment:   ", text x.equipment] 
                      , blockquote [style "font-size" "16px", style "width" "80%"][text "Location:   ", text x.location]]
                    , div [][ blockquote [style "font-size" "20px", style "width" "80%"][text "Job Description:   ", text x.quote] ]
                    , div [][ blockquote [style "font-size" "16px", style "width" "80%"][text "Parts Used:   ", text x.partsUsed] ]
                    , div [][ blockquote [style "font-size" "16px", style "width" "80%"][text "Materials Used:   ", text x.materialsUsed] ]
                    , div [][ blockquote [style "font-size" "16px", style "width" "80%"][text "Further Action:   ", text x.furtherAction] ]
                    , div [] (List.map (\(y, z) ->
                       div []
                       [ blockquote [style "color" "green", style "font-size" "16px"][text y]
                       , p [ style "margin-left" "auto", style "text-align" "right", style "font-size" "14px" ]
                       [ text z]]) x.updateList)
                    ]
                  ] 
                ]) ql

viewDialog : Dialog -> List (Html msg) -> List (Html msg) -> Html msg 
viewDialog dialog html hiddenHtml = 
  case dialog of 
    Closed ->
      div[] hiddenHtml
    Open paragraph ->
      div [] html

viewUpdateForm : String -> String -> (String -> Msg) -> (String -> Msg) -> Html Msg
viewUpdateForm placeholder value callback onclick =
  div []
  [ viewTextArea placeholder value callback 
  , button [id "done-button", onClick (onclick "pointlessString")][text "done"]
  ] 

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
      div [class "inputfield"] [
      input [ type_ t, placeholder p, value v, onInput toMsg] []
      ]

viewRadio : Quote -> Html Msg
viewRadio quote =
     viewPicker 
        [ ((quote.urgency == Low), "low", (SwitchTo quote) Low)
        , ((quote.urgency == Medium), "Medium", (SwitchTo quote) Medium)
        , ((quote.urgency == High), "high", (SwitchTo quote) High)
        ]

viewPicker : List (Bool, String, msg) -> Html msg
viewPicker options =
  fieldset [class "radio-picker"] (List.map radio options)

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

-- HTTP

--GET REQUESTS

getCurrentState: Model -> Cmd Msg
getCurrentState model =
  Http.get
  {
    url = "http://192.168.1.252/server/active"
  , expect = Http.expectJson DataReceived decodeListQuote
  }

decodeListQuote: Decoder (List Quote)
decodeListQuote = Decode.list decodeSingleQuote
    

--  TROUBLE SHOOTING 
decodeSingleQuote: Decoder Quote 
decodeSingleQuote = Decode.succeed Quote
    |> required "quote" Decode.string
    |> required "source" Decode.string
    |> required "author" Decode.string
    |> required "year" Decode.string
    |> hardcoded (Closed)
    |> hardcoded ""
    |> hardcoded Closed
    |> required "updateList" pipeUpdateList
    |> required "urgency" pipeUrgency
    |> required "id" numberToID
    |> required "startTime" Decode.string
    |> required "endTime" Decode.string
    |> required "equipment" Decode.string
    |> required "location" Decode.string
    |> required "partsUsed" Decode.string
    |> required "materialsUsed" Decode.string
    |> required "furtherAction" Decode.string

numberToID:Decoder ID
numberToID = 
  Decode.int |> Decode.andThen (\id -> 
    case id of 
      _ -> Decode.succeed (ID id)
    )

pipeUpdateList: Decoder (List (String, String))
pipeUpdateList = 
  Decode.list decodePair

decodePair: Decoder (String, String)
decodePair = map2 (Tuple.pair) (index 0 Decode.string) (index 1 Decode.string)

pipeUrgency: Decoder Urgency
pipeUrgency = 
  Decode.string |> Decode.andThen (\urgencyString ->
    case urgencyString of 
      "low" -> Decode.succeed Low
      "medium" -> Decode.succeed Medium
      "high" -> Decode.succeed High
      _ -> Decode.succeed ClosedOut
      )



-- POST REQUESTS
postNewRequest: Quote -> Cmd Msg
postNewRequest quote = 
  Http.post
    { 
      body = newRequestEncoder quote |> Http.jsonBody
    , expect = Http.expectWhatever Sent
    , url = "http://192.168.1.252/server/new"
    }
postUpdate: Quote -> Cmd Msg
postUpdate quote = 
  Http.post
    { 
      body = updateEncoder quote |> Http.jsonBody
    , expect = Http.expectWhatever Sent 
    , url = "http://192.168.1.252/server/update"
    }
postStatusChange: Quote -> Cmd Msg
postStatusChange quote = 
  Http.post
    { 
      body = statusEncoder quote |> Http.jsonBody
    , expect = Http.expectWhatever Sent
    , url = "http://192.168.1.252/server/status"
    }
postCloseRequest: Quote -> Cmd Msg
postCloseRequest quote =
  Http.post
    { 
      body = statusEncoder quote |> Http.jsonBody
    , expect = Http.expectWhatever Sent
    , url = "http://192.168.1.252/server/close"
    }

newRequestEncoder : Quote -> Encode.Value
newRequestEncoder quote =
  Encode.object
    [ ("id", encodeId quote.id)
    , ("quote", Encode.string quote.quote)
    , ("source", Encode.string quote.source)
    , ("author", Encode.string quote.author)
    , ("year", Encode.string quote.year)
    , ("updateList", encodeUpdateList quote.updateList)
    , ("urgency", encodeUrgency quote.urgency)
    , ("startTime", Encode.string quote.startTime)
    , ("endTime", Encode.string quote.endTime)
    , ("equipment", Encode.string quote.equipment)
    , ("location", Encode.string quote.location)
    , ("partsUsed", Encode.string quote.partsUsed)
    , ("materialsUsed", Encode.string quote.materialsUsed)
    , ("furtherAction", Encode.string quote.furtherAction)
    ]
updateEncoder : Quote -> Encode.Value
updateEncoder quote =
  Encode.object
    [ ("id", encodeId quote.id)
    , ("updateList", encodeUpdateList quote.updateList)
    , ("urgency", encodeUrgency quote.urgency)
    ]
statusEncoder : Quote -> Encode.Value
statusEncoder quote =
  Encode.object
    [ ("id", encodeId quote.id)
    , ("urgency", encodeUrgency quote.urgency)
    ]
encodeId: ID-> Encode.Value
encodeId id = 
  Encode.int id.id

encodeUpdateList: (List (String, String)) -> Encode.Value
encodeUpdateList updateList = 
  Encode.list identity (List.map 
  (\x -> 
    Encode.list Encode.string [first x, second x]) updateList)

encodeUrgency: Urgency -> Encode.Value
encodeUrgency urgency =
  case urgency of
    Low ->
      Encode.string "low"
    Medium ->
      Encode.string "medium"
    High -> 
      Encode.string "high"
    ClosedOut ->
      Encode.string "closed"


