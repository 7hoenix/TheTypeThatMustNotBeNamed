module Main exposing (..)

import Browser

import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Data
import Dict exposing (Dict)


type Year =
    Year Int

type Month =
    January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December

type alias Model =
    { errors : Maybe String
    , rawYear : String
    , mostPopularBabyName : String
    , monthError : Maybe String
    , rawMonth : String
    }

main =
  Browser.sandbox { init =
    { error = Nothing, rawYear = "", mostPopularBabyName = "", monthError = Nothing, rawMonth = "" }
    , update = update, view = view }

type Msg
    = SetRawYear String
    | SetRawMonth String

update msg model =
  case msg of
    SetRawYear newYear ->
      case yearMeetsComplexBusinessNeeds newYear of
        Ok popularName ->
            { model | rawYear = newYear, mostPopularBabyName = popularName, error = Nothing }
        Err err ->
            { model | rawYear = newYear, mostPopularBabyName = "", error = Just err }

    SetRawMonth rawMonth ->
        let
            error =
                monthMeetsComplexBusinessNeeds rawMonth
        in
        case error of
            Ok month ->
                { model | rawMonth = rawMonth, monthError = Nothing }

            Err e ->
                { model | rawMonth = rawMonth, monthError = Just e }

view model =
  div []
    [ div [] [ text model.mostPopularBabyName]
    , input [ placeholder "Enter year", value model.rawYear, onInput SetRawYear ] []
    , div [] [ text (Maybe.withDefault "" model.error)]
    -- Month stuff
    -- , input [ placeholder "Enter month", value model.rawMonth, onInput SetRawMonth ] []
    -- , div [] [ text (Maybe.withDefault "" model.monthError)]
    ]


type alias ValidBirthday =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    }

validMonthOfYear : Int -> Result String Int
validMonthOfYear rawMonth =
    if rawMonth >= 1 && rawMonth <= 12 then
        Ok rawMonth -- We are going to use the "Ok" constructor (Which is expecting an Int).
    else
        Err (String.fromInt rawMonth ++ " is not a valid month!") -- We are going to use the "Err" constructor (Which needs a String).

yearNotTooFarInThePast : Int -> Result String Int
yearNotTooFarInThePast rawYear =
    if rawYear >= 0 then
        Ok rawYear
    else
        Err (String.fromInt rawYear ++ " Is before 0 AD!")

yearInTimePeriod : Int -> Result String Int
yearInTimePeriod rawYear =
    if 1919 <= rawYear && rawYear <= 2018 then
        Ok rawYear
    else
        Err ("You must select a year between 1919 and 2018.")

yearNotIn90s : Int -> Result String Int
yearNotIn90s rawYear =
    if rawYear < 800 || rawYear > 1300 then
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


monthMeetsComplexBusinessNeeds : String -> Result String Month
monthMeetsComplexBusinessNeeds rawMonth =
     toMonth rawMonth
        |> Result.andThen hasThirtyDays 

-- type YearAndMonth =
--     YearAndMonth Year Month
    
-- validateRequest : String -> String -> Result String YearAndMonth
-- validateRequest rawYear rawMonth =
--     Result.map2 YearAndMonth
--         (yearMeetsComplexBusinessNeeds rawYear)
--         (monthMeetsComplexBusinessNeeds rawMonth)


-- PRIVATE

hasThirtyDays : Month -> Result String Month
hasThirtyDays month =
    case month of
        September ->
            Ok month
        April ->
            Ok month
        June ->
            Ok month
        November ->
            Ok month
        monthDoesntHave30 ->
            Err <| Debug.toString monthDoesntHave30 ++ " doesn't have 30 days."

toMonth : String -> Result String Month
toMonth rawMonth =
     case rawMonth of
        "jan" -> Ok January
        "feb" -> Ok February
        "mar" -> Ok March
        "apr" -> Ok April
        "may" -> Ok May
        "jun" -> Ok June
        "jul" -> Ok July
        "aug" -> Ok August
        "sep" -> Ok September
        "oct" -> Ok October
        "nov" -> Ok November
        "dec" -> Ok December
        raw -> Err <| raw ++ " is not a valid month!"


-- validateBirthdayWithHour : Int -> Int -> Int -> Int -> Result String ValidBirthday
-- validateBirthdayWithHour year month day hour =
--     isValidYear year
--       |> Result.andThen (validMonthOfYear month)
--       |> Debug.todo "fail"
--         |> (\year month -> Result.andThen isValidDay day)
--         |> (\year month day -> Result.andThen isValidHour hour)
--         |> ValidBirthday