port module PasswordGen exposing (main)

import Browser
import Html exposing (Html, button, div, fieldset, form, input, label, legend, span, text)
import Html.Attributes as Attribute exposing (checked, class, disabled, for, id, placeholder, required, selected, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


lowerCaseAbc =
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]


upperCaseAbc =
    List.map Char.toUpper lowerCaseAbc


numbers =
    [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' ]


additionalChars =
    [ '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+', '=', '[', ']', '{', '}', ':', ';' ]


type alias Model =
    { password : Maybe String
    , passwordLength : Int
    , passwordOptions : PasswordOptions
    , submittedPassword : Bool
    , startedClock : Bool
    , clockInSeconds : Int
    }


type alias PasswordOptions =
    { useUppercase : Bool
    , useAdditionalChars : Bool
    , useNumbers : Bool
    , showPassword : Bool
    }


type Msg
    = OnSubmit
    | GeneratedPassword (List Char)
    | PasswordLength String
    | ToggleUseUppercase
    | ToggleUseAdditionalChars
    | ToggleUseNumbers
    | ToggleShowPassword
    | CopyPassword
    | Tick Time.Posix
    | Reset



-- INIT


init : () -> ( Model, Cmd msg )
init _ =
    ( Model Nothing 8 (PasswordOptions False False False False) False False 0
    , Cmd.none
    )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    if (model.startedClock) then
        Time.every 1000 Tick
    else
        Sub.none

-- PORTS

port copyPasswordToClipboard : String -> Cmd msg

port resetPage : () -> Cmd msg
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit ->
            let
                uniformValuesGenerator : Random.Generator (List Char)
                uniformValuesGenerator =
                    getRandomGenerator model.passwordLength (effectiveListOfPossibleValues model.passwordOptions)
            in
            ( {model | startedClock = True, clockInSeconds = 0 }, Random.generate GeneratedPassword uniformValuesGenerator )

        GeneratedPassword selectedPasswordChars ->
            ( { model | password = selectedPasswordChars |> List.map String.fromChar |> String.concat |> Just }, Cmd.none )

        PasswordLength passwordLengthStr ->
            ( { model | passwordLength = String.toInt passwordLengthStr |> Maybe.withDefault 8 }, Cmd.none )

        ToggleUseUppercase ->
            let
                passwordOptions =
                    model.passwordOptions

                newPasswordOptions =
                    { passwordOptions | useUppercase = not passwordOptions.useUppercase }
            in
            ( { model | passwordOptions = newPasswordOptions }, Cmd.none )

        ToggleUseAdditionalChars ->
            let
                passwordOptions =
                    model.passwordOptions

                newPasswordOptions =
                    { passwordOptions | useAdditionalChars = not passwordOptions.useAdditionalChars }
            in
            ( { model | passwordOptions = newPasswordOptions }, Cmd.none )

        ToggleUseNumbers ->
            let
                passwordOptions =
                    model.passwordOptions

                newPasswordOptions =
                    { passwordOptions | useNumbers = not passwordOptions.useNumbers }
            in
            ( { model | passwordOptions = newPasswordOptions }, Cmd.none )

        ToggleShowPassword ->
            let
                passwordOptions =
                    model.passwordOptions

                newPasswordOptions =
                    { passwordOptions | showPassword = not passwordOptions.showPassword }
            in
            ( { model | passwordOptions = newPasswordOptions }, Cmd.none )

        CopyPassword ->
            case model.password of
                Just password ->
                    (model, copyPasswordToClipboard password)
                Nothing ->
                    (model, copyPasswordToClipboard "")

        Tick posix ->
            if (model.clockInSeconds >= 20) then
                update Reset model
             else
                ({model | clockInSeconds = model.clockInSeconds + 1}, Cmd.none)

        Reset ->
            (model, resetPage ())




effectiveListOfPossibleValues : PasswordOptions -> List Char
effectiveListOfPossibleValues passwordOptions =
    let
        baseList =
            lowerCaseAbc

        listWithUppercase =
            if passwordOptions.useUppercase then
                baseList ++ upperCaseAbc

            else
                baseList

        listWithNumbers =
            if passwordOptions.useNumbers then
                listWithUppercase ++ numbers

            else
                listWithUppercase
    in
    if passwordOptions.useAdditionalChars then
        listWithNumbers ++ additionalChars

    else
        listWithNumbers


getRandomGenerator : Int -> List Char -> Random.Generator (List Char)
getRandomGenerator passwordLength possibleValues =
    case possibleValues of
        [] ->
            Random.list passwordLength (Random.constant 'A')

        firstValue :: restOfValues ->
            Random.list passwordLength (Random.uniform firstValue restOfValues)



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log (Maybe.withDefault "hatul!" model.password)
    in
    div [ class "pure-g" ]
        [
        div [class "pure-u-1-6" ][]
        ,div [ class "pure-u-1-4" ]
            [ form [ class "pure-form pure-form-stacked", onSubmit OnSubmit ]
                [ fieldset []
                    [ legend []
                        [ text "A Stacked Form" ]
                    , label [ for "stacked-email" ] [ text "Password Length" ]
                    , input
                        [ id "stacked-email"
                        , placeholder "9"
                        , type_ "number"
                        , Attribute.min "8"
                        , Attribute.max "10"
                        , required True
                        , style "width" "60px"
                        , onInput PasswordLength
                        ]
                        []
                    , span [ class "pure-form-message" ]
                        [ text "This is a required field." ]
                    , label [ class "pure-checkbox", for "use-uppercase" ]
                        [ input [ id "use-uppercase", type_ "checkbox", onClick ToggleUseUppercase, checked model.passwordOptions.useUppercase]
                            []
                        , text "also use uppercase letters"
                        ]
                    , label [ class "pure-checkbox", for "use-numbers" ]
                        [ input [ id "use-numbers", type_ "checkbox", onClick ToggleUseNumbers, checked model.passwordOptions.useNumbers ]
                            []
                        , text "also use numbers"
                        ]
                    , label [ class "pure-checkbox", for "additional-characters" ]
                        [ input [ id "additional-characters", type_ "checkbox", onClick ToggleUseAdditionalChars, checked model.passwordOptions.useAdditionalChars]
                            []
                        , text "use additional ASCII characters"
                        ]
                    , button [ class "pure-button pure-button-primary", type_ "submit" ]
                        [ text "Generate Password" ]
                    ]
                ]
            ]
        , div [ class "pure-u-1-4" ] [ displayPassword model.passwordOptions.showPassword model.password ]
        , if (model.startedClock) then
            text (String.fromInt model.clockInSeconds)
          else
            text ""
        ]


displayPassword : Bool -> Maybe String -> Html Msg
displayPassword showPassword possiblePassword =
    case possiblePassword of
        Just password ->
            div []
                [ label [ class "pure-checkbox", for "show-password" ]
                    [ input [ id "how-password", type_ "checkbox", onClick ToggleShowPassword ][]
                    ]
                    ,
                    if (showPassword) then
                        input [type_ "text", value password, disabled True][]
                    else
                        input [type_ "password", value password, disabled True][]
                    , button [onClick CopyPassword ] [ text "copy password!"]
                ]
        Nothing ->
            text "still no password"
