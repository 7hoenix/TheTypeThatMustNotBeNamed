module Main exposing (..)

import Browser
import Data
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (placeholder)
import Html.Events exposing (onInput)
import Html exposing (Html)


type Year
    = Year Int


type alias Model =
    { error : Maybe String
    , rawYear : String
    , mostPopularBabyName : String
    }


main =
    Browser.sandbox
        { init =
            { error = Nothing, rawYear = "", mostPopularBabyName = "" }
        , update = update
        , view = view
        }


type Msg
    = SetRawYear String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetRawYear newYear ->
            case yearMeetsComplexBusinessNeeds newYear of
                Ok popularName ->
                    { model | rawYear = newYear, mostPopularBabyName = popularName, error = Nothing }

                Err err ->
                    { model | rawYear = newYear, mostPopularBabyName = "", error = Just err }


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.color (rgba 0 0 0 1)
        , Font.italic
        , Font.size 32
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=EB+Garamond"
                , name = "EB Garamond"
                }
            , Font.sansSerif
            ]
        ]
    <|
        el
            [ centerX, centerY ]
            (Element.column
                []
                [ Input.text []
                    { placeholder = Just (Input.placeholder [] (text "1919"))
                    , text = model.rawYear
                    , onChange = SetRawYear
                    , label = Input.labelAbove [ Font.size 14 ] (text "Enter a year!")
                    }
                , el [] (Element.text model.mostPopularBabyName)
                , el [] (text (Maybe.withDefault "" model.error))
                ]
            )

yearInTimePeriod : Int -> Result String Int
yearInTimePeriod rawYear =
    if 1919 <= rawYear && rawYear <= 2018 then
        Ok rawYear
    else
        Err ("You must select a year between 1919 and 2018.")

yearNotIn90s : Int -> Result String Int
yearNotIn90s rawYear =
    if rawYear < 1990 || rawYear > 1999 then
        Ok rawYear

    else
        Err (String.fromInt rawYear ++ " is in the 90s.")


yearMeetsComplexBusinessNeeds : String -> Result String String
yearMeetsComplexBusinessNeeds rawYear =
    String.toInt rawYear
        |> Result.fromMaybe "Years must be numbers!"
        |> Result.andThen yearInTimePeriod
        |> Result.andThen yearNotIn90s
        |> Result.andThen findMostPopularBabyName


popularNames : Dict Int String
popularNames =
    Dict.fromList Data.popularNames


findMostPopularBabyName : Int -> Result String String
findMostPopularBabyName year =
    Dict.get year popularNames
        |> Result.fromMaybe ("We don't have data for " ++ String.fromInt year ++ "!")
