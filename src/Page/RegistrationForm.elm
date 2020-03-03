module Page.RegistrationForm exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, form, input, label, li, strong, text, ul)
import Html.Attributes exposing (class, for, id, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Body, multipartBody, stringPart)
import Layout exposing (col)
import Regex
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query
import Validate exposing (Validator, fromErrors, validate)



-- MODEL


type Field
    = Username String
    | Password String
    | PasswordConfirmation String
    | Token String


type alias Model =
    { username : Field
    , token : Field
    , password : Field
    , passwordConfirmation : Field
    , errors : List String
    , success : Maybe (Result Http.Error ())
    }


init : Url -> Model
init url =
    { username = Username ""
    , password = Password ""
    , passwordConfirmation = PasswordConfirmation ""
    , token = Parser.parse tokenParser url |> Maybe.withDefault Nothing |> Maybe.withDefault "" |> Token
    , errors = []
    , success = Nothing
    }



-- UPDATE


type Msg
    = FormSubmit
    | EnteredField Field
    | CompletedRegistration (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormSubmit ->
            case validate formValidator model of
                Ok validForm ->
                    ( { model | errors = [] }, register (Validate.fromValid validForm) )

                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

        CompletedRegistration result ->
            case result of
                Ok _ ->
                    ( { model | success = Just (Ok ()) }, Cmd.none )

                Err error ->
                    ( { model | success = Just (Err error) }, Cmd.none )

        EnteredField field ->
            case field of
                Username value ->
                    ( { model | username = Username value }, Cmd.none )

                Password value ->
                    ( { model | password = Password value }, Cmd.none )

                PasswordConfirmation value ->
                    ( { model | passwordConfirmation = PasswordConfirmation value }, Cmd.none )

                Token value ->
                    ( { model | token = Token value }, Cmd.none )



-- VIEW


inputGroup : Field -> Html Msg
inputGroup field =
    let
        ( input_, label_, field_ ) =
            case field of
                Username _ ->
                    ( "text", "Username", Username )

                Password _ ->
                    ( "password", "Password", Password )

                PasswordConfirmation _ ->
                    ( "password", "Confirmation", PasswordConfirmation )

                Token _ ->
                    ( "text", "Token", Token )
    in
    div [ class "form-group" ]
        [ label [ class "form-label label-lg", for label_ ] [ text label_ ]
        , input
            [ type_ input_
            , id label_
            , onInput <| EnteredField << field_
            , value <| fieldToString field
            , required True
            , class "s-rounded form-input"
            ]
            []
        ]


displayErrors : List String -> Html msg
displayErrors errors =
    List.map (\error -> li [] [ strong [] [ text error ] ]) errors |> ul [ class "text-error" ]


displaySuccess : Model -> Html msg
displaySuccess model =
    let
        layout : Html msg -> Html msg
        layout children =
            div [ class "columns" ]
                [ col 6
                    "col-mx-auto"
                    [ div [ class "panel" ]
                        [ div [ class "panel-body text-center" ]
                            [ children ]
                        ]
                    ]
                ]
    in
    case model.success of
        Nothing ->
            div [] []

        Just result ->
            case result of
                Ok _ ->
                    Html.span [ class "text-success" ] [ text "Great success!" ] |> layout

                Err error ->
                    case error of
                        Http.BadUrl _ ->
                            text "BadUrl" |> layout

                        Http.Timeout ->
                            text "Timeout" |> layout

                        Http.NetworkError ->
                            text "NetworkError" |> layout

                        Http.BadStatus status ->
                            text ("BadStatus " ++ String.fromInt status) |> layout

                        Http.BadBody body ->
                            text ("BadBody " ++ body) |> layout


view : Model -> Html Msg
view model =
    col 4
        "col-mx-auto col-sm-12"
        [ displaySuccess model
        , form [ onSubmit FormSubmit ]
            [ inputGroup model.username
            , inputGroup model.password
            , inputGroup model.passwordConfirmation
            , inputGroup model.token
            , div [ class "mt-2" ] [ button [ type_ "submit", class "btn btn-lg s-rounded" ] [ text "Register" ] ]
            , displayErrors model.errors
            ]
        ]



-- HELPERS


tokenParser : Parser (Maybe String -> a) a
tokenParser =
    Parser.s "registration" <?> Query.string "token"


formValidator : Validator String Model
formValidator =
    fromErrors formErrors


formErrors : Model -> List String
formErrors model =
    [ model.username
    , model.password
    , model.passwordConfirmation
    , model.token
    ]
        |> List.concatMap (mapFormErrors model)


mapFormErrors : Model -> Field -> List String
mapFormErrors model field =
    let
        errors =
            []
    in
    case field of
        Username value ->
            let
                pattern =
                    Regex.fromString "^@?[a-zA-Z_\\-=\\.\\/0-9]+(:matrix\\.org)?$" |> Maybe.withDefault Regex.never
            in
            if String.isEmpty value then
                errors ++ [ "Username is required" ]

            else if String.length value > 30 then
                errors ++ [ "Username cannot be more than 30 characaters" ]

            else if not (Regex.contains pattern value) then
                errors ++ [ "Username must follow the format of @user:matrix.lgbt" ]

            else
                errors

        Password value ->
            if String.isEmpty value then
                errors ++ [ "Password is required" ]

            else if String.length value < 8 then
                errors ++ [ "Password must be at least 8 characters" ]

            else if String.length value > 128 then
                errors ++ [ "Password cannot be more than 128 characters" ]

            else
                errors

        PasswordConfirmation value ->
            if value /= fieldToString model.password then
                errors ++ [ "Password confirmation does not match password" ]

            else
                errors

        Token value ->
            if String.isEmpty value then
                errors ++ [ "Registration token is required" ]

            else
                errors


fieldToString : Field -> String
fieldToString field =
    case field of
        Username value ->
            value

        Password value ->
            value

        PasswordConfirmation value ->
            value

        Token value ->
            value


register : Model -> Cmd Msg
register model =
    let
        toMultipart : Body
        toMultipart =
            multipartBody
                [ stringPart "username" <| fieldToString model.username
                , stringPart "password" <| fieldToString model.password
                , stringPart "confirm" <| fieldToString model.passwordConfirmation
                , stringPart "token" <| fieldToString model.token
                ]
    in
    Http.post
        { url = "https://matrix.lgbt/register"
        , body = toMultipart
        , expect = Http.expectWhatever CompletedRegistration
        }
