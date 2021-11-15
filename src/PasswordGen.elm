module PasswordGen exposing (main)

import Browser
import Html exposing (Html, button, div, fieldset, form, h1, input, label, legend, option, select, span, text)
import Html.Attributes as Attribute exposing (checked, class, for, id, placeholder, required, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random



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
    }


type alias PasswordOptions =
    { useUppercase : Bool
    , useAdditionalChars : Bool
    }


type Msg
    = OnSubmit
    | GeneratedPassword (List Char)
    | PasswordLength String
    | ToggleUseUppercase
    | ToggleUseAdditionalChars



-- INIT


init : () -> ( Model, Cmd msg )
init _ =
    ( Model Nothing 8 (PasswordOptions False False)
    , Cmd.none
    )



-- SUBSCRIPTION


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit ->
            let
                uniformValuesGenerator : Random.Generator (List Char)
                uniformValuesGenerator = getRandomGenerator model.passwordLength (effectiveListOfPossibleValues model.passwordOptions)
            in
            ( model, Random.generate GeneratedPassword uniformValuesGenerator )

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


effectiveListOfPossibleValues: PasswordOptions -> List Char
effectiveListOfPossibleValues passwordOptions =
    let
        baseList = lowerCaseAbc
        listWithUppercase = if (passwordOptions.useUppercase) then baseList ++ upperCaseAbc else baseList
    in
        if (passwordOptions.useAdditionalChars) then
            listWithUppercase ++ additionalChars
         else
            listWithUppercase

getRandomGenerator: Int -> List Char -> Random.Generator (List Char)
getRandomGenerator passwordLength possibleValues =
     case possibleValues of
         [] -> Random.list passwordLength (Random.constant 'A')
         firstValue :: restOfValues -> Random.list passwordLength (Random.uniform firstValue restOfValues)

-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log (Maybe.withDefault "hatul!" model.password)
    in
    div [ class "pure-g" ]
        [ div [ class "pure-u-1-3" ]
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
                        [ input [ id "use-uppercase", type_ "checkbox", onClick ToggleUseUppercase ]
                            []
                        , text "also user uppercase letters"
                        ]
                    , label [ class "pure-checkbox", for "additional-characters" ]
                        [ input [ id "additional-characters", type_ "checkbox", onClick ToggleUseAdditionalChars ]
                            []
                        , text "use additional ASCII characters"
                        ]
                    , button [ class "pure-button pure-button-primary", type_ "submit" ]
                        [ text "Generate Password" ]
                    ]
                ]
            ]
        , div [ class "pure-u-2-3" ] [ displayPassword model.password ]
        ]


displayPassword : Maybe String -> Html msg
displayPassword possiblePassword =
    case possiblePassword of
        Just password ->
            text ("here is the password: " ++ password)

        Nothing ->
            text "still no password"
