module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Set



-- elm-package install --yes circuithub/elm-json-extra
-- MAIN


type alias Flags =
    { studios_url : String }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { title : Maybe String
    , city : Maybe String
    , studio_type : Maybe String
    , studios : Maybe (List Studio)
    , studio_types : Maybe (List String)
    , list_amount : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model Nothing Nothing Nothing Nothing Nothing 100
    , getStudios flags.studios_url
    )



-- UPDATE


type Msg
    = FilterCity String
    | FilterType String
    | FilterTitle String
    | ShowStudios (Result Http.Error (List Studio))
    | SetListAmount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilterCity city ->
            ( { model | city = Just city }, Cmd.none )

        FilterType type_ ->
            ( { model | studio_type = Just type_ }, Cmd.none )

        FilterTitle title ->
            ( { model | title = Just title }, Cmd.none )

        SetListAmount amount ->
            ( { model | list_amount = Maybe.withDefault 100 (String.toInt amount) }, Cmd.none )

        ShowStudios result ->
            case result of
                Ok studios ->
                    ( { model
                        | studios = Just studios
                        , studio_types =
                            Just (List.map (\studio -> studio.studio_type) studios |> Set.fromList |> Set.toList)
                      }
                    , Cmd.none
                    )

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
            [ text "Title"
            , input [ onInput FilterTitle, type_ "search" ] []
            ]
        , label []
            [ text "City"
            , input [ onInput FilterCity, type_ "search" ] []
            ]
        , showTypeSelect model.studio_types
        , label []
            [ text "Show First"
            , input [ type_ "number", onInput SetListAmount, value (String.fromInt model.list_amount) ] []
            ]
        , showStudios model
        ]


bodyStyle : List (Attribute msg)
bodyStyle =
    [ style "margin" "2rem"
    ]


showTypeSelect : Maybe (List String) -> Html Msg
showTypeSelect maybe_studio_types =
    case maybe_studio_types of
        Nothing ->
            label [] [ text "Type", select [] [] ]

        Just studio_types ->
            label []
                [ text "Type"
                , select [ onInput FilterType ]
                    (List.map
                        (\studio_type -> option [] [ text studio_type ])
                        studio_types
                        |> List.append
                            [ option
                                []
                                [ text "ALL" ]
                            ]
                    )
                ]


showStudios : Model -> Html Msg
showStudios model =
    case model.studios of
        Nothing ->
            div [] [ text "No Studios found" ]

        Just studios ->
            let
                filtered_studios =
                    studios
                        |> filterStudiosOnCity model.city
                        |> filterStudiosOnType model.studio_type
                        |> filterStudiosOnTitle model.title
            in
            div []
                [ h4 [] [ text ("Show the first " ++ String.fromInt model.list_amount ++ " from " ++ String.fromInt (List.length filtered_studios)) ]
                , Keyed.node "ol" [] (List.map showKeyedStudio (List.take model.list_amount filtered_studios))
                , viewExpandButton model filtered_studios
                ]


viewExpandButton : Model -> List Studio -> Html Msg
viewExpandButton model studios =
    if model.list_amount <= List.length studios then
        button [ onClick (SetListAmount (String.fromInt (model.list_amount + 100))) ] [ text "ADD 100" ]

    else
        button [ disabled True ] [ text "ADD 100" ]


filterStudiosOnType : Maybe String -> List Studio -> List Studio
filterStudiosOnType maybe_type studios =
    case maybe_type of
        Nothing ->
            studios

        Just type_ ->
            if type_ == "ALL" then
                studios

            else
                List.filter (\studio -> String.contains (String.toLower type_) (String.toLower studio.studio_type)) studios


filterStudiosOnCity : Maybe String -> List Studio -> List Studio
filterStudiosOnCity maybe_city studios =
    case maybe_city of
        Nothing ->
            studios

        Just city ->
            List.filter (\studio -> String.contains (String.toLower city) (String.toLower studio.city)) studios


filterStudiosOnTitle : Maybe String -> List Studio -> List Studio
filterStudiosOnTitle maybe_title studios =
    case maybe_title of
        Nothing ->
            studios

        Just title ->
            List.filter (\studio -> String.contains (String.toLower title) (String.toLower studio.title)) studios


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
                [ text (emojiForType studio.studio_type ++ " " ++ studio.title)
                ]
            , p [] [ b [] [ text "Type: " ], text studio.studio_type ]
            , p [] [ b [] [ text "City: " ], text studio.city ]
            , p [] [ b [] [ text "Adress: " ], text studio.address ]
            ]
        ]


emojiForType : String -> String
emojiForType studio_type =
    case studio_type of
        "GYM" ->
            "💪"

        "SWIMMING_POOL" ->
            "🏊\u{200D}♀️"

        "PHYSIO" ->
            "👩\u{200D}⚕️"

        "BOULDER_HALL" ->
            "🧗\u{200D}♀️"

        "CROSSFIT" ->
            "☠️"

        "YOGA_STUDIO" ->
            "🧘\u{200D}♀️"

        _ ->
            "𖡄"


getStudios : String -> Cmd Msg
getStudios studiosUrl =
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
