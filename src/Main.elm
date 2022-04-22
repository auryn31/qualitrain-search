module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, type_, value)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { name: String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Hello World"
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div bodyStyle
        [ h1 [] [ text "First Project" ]
        , p [] [text model.name]
        ]




bodyStyle : List (Attribute msg)
bodyStyle =
    [ style "margin" "2rem"
    ]


