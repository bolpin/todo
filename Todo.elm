port module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task


main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- -- MODEL


type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }



-- UPDATE


type Msg
    = NoOp
    | UpdateField String
    | Add
    | SetState Int Bool
    | ChangeVisibility String
    | CheckAll Bool
    | DeleteAllCompleted


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel, Cmd.batch [ setStorage newModel, cmds ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        CheckAll isCompleted ->
            let
                updateCompletedState task =
                    { task | completed = isCompleted }
            in
                { model | entries = List.map updateCompletedState model.entries }
                    ! []

        DeleteAllCompleted ->
            { model | entries = List.filter (not << .completed) model.entries }
                ! []

        UpdateField entry ->
            { model | field = entry } ! []

        ChangeVisibility newVisibility ->
            { model | visibility = newVisibility } ! []

        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ newEntry model.field model.uid ]
            }
                ! []

        SetState id newState ->
            let
                updateEntry task =
                    if task.id == id then
                        { task | completed = newState }
                    else
                        task
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []


newEntry : String -> Int -> Entry
newEntry entry uid =
    { description = entry
    , completed = False
    , editing = False
    , id = uid
    }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style [ ( "visibility", "hidden" ) ]
        ]
        [ section
            [ class "todoapp" ]
            [ lazy renderInput model.field
            , lazy2 renderEntries model.visibility model.entries
            , lazy2 renderControls model.visibility model.entries
            ]
        ]


renderControls : String -> List Entry -> Html Msg
renderControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
        footer
            [ class "footer"
            , hidden (List.isEmpty entries)
            ]
            [ lazy renderControlsCount entriesLeft
            , lazy renderControlsVisibility visibility
            , lazy renderDeleteControl entriesCompleted
            ]


renderInput : String -> Html Msg
renderInput task =
    div []
        [ input
            [ class "new-todo"
            , placeholder "Enter a new task"
            , value task
            , autofocus True
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


renderEntries : String -> List Entry -> Html Msg
renderEntries visibility entries =
    let
        isVisible task =
            case visibility of
                "Completed" ->
                    task.completed

                "Active" ->
                    not task.completed

                _ ->
                    True

        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"

        allCompleted =
            List.all .completed entries
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class "todo-list" ] <|
                List.map
                    renderKeyedEntry
                    (List.filter isVisible entries)
            ]


renderKeyedEntry : Entry -> ( String, Html Msg )
renderKeyedEntry task =
    ( toString task.id, lazy renderEntry task )


renderEntry : Entry -> Html Msg
renderEntry entry =
    let
        newState oldState =
            if oldState then
                False
            else
                True
    in
        li
            [ onClick (SetState entry.id (newState entry.completed))
            , style
                [ ( "text-decoration", getTaskTextDecoration entry.completed )
                , ( "cursor", "pointer" )
                ]
            ]
            [ text entry.description ]


getTaskTextDecoration : Bool -> String
getTaskTextDecoration completed =
    if completed then
        "line-through"
    else
        "none"


renderControlsCount : Int -> Html Msg
renderControlsCount count =
    span [ class "todo-count" ] [ text (toString count ++ " remaining") ]


renderControlsVisibility : String -> Html Msg
renderControlsVisibility visibility =
    ul [ class "filters" ]
        [ visibilitySwitch "#/" "All" visibility
        , visibilitySwitch "#/active" "Active" visibility
        , visibilitySwitch "#/completed" "Completed" visibility
        ]


renderDeleteControl : Int -> Html Msg
renderDeleteControl entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteAllCompleted
        ]
        [ text "Clear All Completed Tasks" ]


visibilitySwitch : String -> String -> String -> Html Msg
visibilitySwitch uri newVisibility actualVisibility =
    li [ onClick (ChangeVisibility newVisibility) ]
        [ a
            [ href uri
            , classList [ ( "selected", newVisibility == actualVisibility ) ]
            ]
            [ text newVisibility ]
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
