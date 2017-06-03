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


main =
    Html.program
        { init = init (Just emptyModel)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []


type Msg
    = NoOp
    | Input String
    | UpdateField String
    | Add
    | SetState Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Input str ->
            model ! []

        UpdateField entry ->
            { model | field = entry } ! []

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
        , style [ ( "visibility", "" ) ]
        ]
        [ section
            [ class "todoapp" ]
            [ lazy renderInput model.field
            , lazy renderEntries model.entries
            , lazy2 renderControls model.visibility model.entries
            ]
        ]


renderInput : String -> Html Msg
renderInput task =
    div []
        [ input
            [ class "new-todo"
            , placeholder "Enter an item"
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


renderEntries : List Entry -> Html Msg
renderEntries entries =
    ul []
        (List.map
            renderEntry
            entries
        )


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
                [ ( "text-decoration", getTaskColor entry.completed )
                , ( "cursor", "pointer" )
                ]
            ]
            [ text entry.description ]


getTaskColor : Bool -> String
getTaskColor completed =
    if completed then
        "line-through"
    else
        "none"


renderControls : String -> List Entry -> Html Msg
renderControls visibility entries =
    ul [ class "filters" ]
        [ changeVisibility "#/" "All" visibility
        , changeVisibility "#/active" "Active" visibility
        , changeVisibility "#/completed" "Completed" visibility
        ]


changeVisibility : String -> String -> String -> Html Msg
changeVisibility uri newVisibility actualVisibility =
    li []
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
