module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Decode
import Json.Decode.Pipeline



-- elm-package install --yes circuithub/elm-json-extra
-- MAIN


studiosUrl =
    "/resources/studios.json"


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { city : Maybe String
    , studios : Maybe (List Studio)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing
    , getStudios
    )



-- UPDATE


type Msg
    = SearchCity String
    | GetStudios
    | ShowStudios (Result Http.Error (List Studio))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchCity city ->
            ( { model | city = Just city }, Cmd.none )

        GetStudios ->
            ( model, Cmd.none )

        ShowStudios result ->
            case result of
                Ok studios ->
                    ( { model | studios = Just studios }, Cmd.none )

                Err _ ->
                    ( { model | studios = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div bodyStyle
        [ h1 [] [ text "Qualitrain Studios" ]
        , label []
            [ text "City"
            , input [ onInput SearchCity ] []
            ]
        , p [] [ text ("City search for: " ++ Maybe.withDefault "-" model.city) ]
        , showStudios model.studios model.city
        ]


bodyStyle : List (Attribute msg)
bodyStyle =
    [ style "margin" "2rem"
    ]


showStudios : Maybe (List Studio) -> Maybe String -> Html msg
showStudios maybe_studios maybe_city =
    case maybe_studios of
        Nothing ->
            div [] [ text "No Studios found" ]

        Just studios ->
            case maybe_city of
                Nothing ->
                    div []
                        [ h1 [] [ text ("All " ++ String.fromInt (List.length studios) ++ " Studios:") ]
                        , Keyed.node "ol" [] (List.map showKeyedStudio studios)
                        ]

                Just city ->
                    let
                        filtered_studios =
                            filterStudiosOnCity studios city
                    in
                    div []
                        [ h1 [] [ text ("Found " ++ String.fromInt (List.length filtered_studios) ++ " Studios:") ]
                        , Keyed.node "ol" [] (List.map showKeyedStudio filtered_studios)
                        ]


filterStudiosOnCity : List Studio -> String -> List Studio
filterStudiosOnCity studios city =
    List.filter (\studio -> String.contains (String.toLower city) (String.toLower studio.city)) studios


showKeyedStudio : Studio -> ( String, Html msg )
showKeyedStudio studio =
    ( studio.nameurlslug
    , lazy showStudio studio
    )


showStudio : Studio -> Html msg
showStudio studio =
    li []
        [ details []
            [ summary []
                [ text studio.title
                ]
            , p [] [ text studio.city ]
            ]
        ]


getStudios : Cmd Msg
getStudios =
    Http.get
        { url = studiosUrl
        , expect = Http.expectJson ShowStudios decodeStudio
        }


type alias Studio =
    { cityurlslug : String
    , nameurlslug : String
    , title : String
    , studio_type : String
    , address : String
    , city : String
    , zipcode : String
    , country : String
    , lat : String
    , lng : String
    }


decodeStudio : Json.Decode.Decoder (List Studio)
decodeStudio =
    Json.Decode.list
        (Json.Decode.succeed Studio
            |> Json.Decode.Pipeline.required "cityurlslug" Json.Decode.string
            |> Json.Decode.Pipeline.required "nameurlslug" Json.Decode.string
            |> Json.Decode.Pipeline.required "title" Json.Decode.string
            |> Json.Decode.Pipeline.required "type" Json.Decode.string
            |> Json.Decode.Pipeline.required "address" Json.Decode.string
            |> Json.Decode.Pipeline.required "city" Json.Decode.string
            |> Json.Decode.Pipeline.required "zipcode" Json.Decode.string
            |> Json.Decode.Pipeline.required "country" Json.Decode.string
            |> Json.Decode.Pipeline.required "lat" Json.Decode.string
            |> Json.Decode.Pipeline.required "lng" Json.Decode.string
        )
