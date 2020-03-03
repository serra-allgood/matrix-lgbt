module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, details, div, em, h2, i, strong, summary, text)
import Html.Attributes exposing (class, href)
import Layout exposing (col, layout)
import Page.RegistrationForm as Register
import Url
import Url.Parser as Parser exposing (Parser)



-- MODEL


type Page
    = HomePage
    | RegisterPage


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , registrationForm : Register.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { page = Parser.parse routeParser url |> Maybe.withDefault HomePage
      , key = key
      , url = url
      , registrationForm = Register.init url
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotRegisterMsg Register.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, page = Maybe.withDefault HomePage <| Parser.parse routeParser url }, Cmd.none )

        GotRegisterMsg registerMsg ->
            let
                ( registrationForm, cmd ) =
                    Register.update registerMsg model.registrationForm
            in
            ( { model | registrationForm = registrationForm }, Cmd.map GotRegisterMsg cmd )


routeParser : Parser (Page -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomePage Parser.top
        , Parser.map RegisterPage (Parser.s "registration")
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        HomePage ->
            layout "Home" (homeView model)

        RegisterPage ->
            layout "Register" [ Html.map GotRegisterMsg (Register.view model.registrationForm) ]


homeView : Model -> List (Html msg)
homeView _ =
    [ col 3
        "col-ml-auto col-xs-12"
        [ accordion "What is Matrix?" [ a [ href "https://matrix.org" ] [ text "Matrix" ], text " is an open protocol for decentralized communication that supports end-to-end encryption, and it has the concept of \"bridging\" to other communication platforms, such as Telegram, WhatsApp, Discord, Slack, IRC, and many others." ]
        , accordion "What does it mean for Matrix to be decentralized?" [ text "Decentralized means no single entity controls all of the data that makes up the Matrix network. When a user joins a chat room, the home server they belong to talks to the other servers in the room and gets the complete state of the room." ]
        , accordion "How does chat work in Matrix?" [ text "Like most chat platforms, really. The basic organizational concept in Matrix is a ", em [] [ text "room" ], text ". Rooms can be end-to-end encrypted, and depending on which Matrix client you use, you can pretty seamlessly automatically manage your encryption keys. As the specification for Matrix matures, the concept of a room is expanding beyond just chat into many features you see on other platforms, such as 1) \"group as a room\" that will enable Discord-server like organization as well as hypothetical forum-like capabilities, 2) user profiles, 3) mute lists to which you can subscribe, and much more. The communication protocol is being actively explored by a wide ecosystem, and one particular project that highlights the flexibiliy of Matrix is matrix-notepad, a project for collaborative document editing via Matrix." ]
        , accordion "Is anybody really using Matrix?" [ text "Remarkably, yes! Mozilla recently announced they've officially chosen Matrix as their replacement for IRC, and the German Ministry of Defense as well as the French government have adopted it too!" ]
        ]
    , col 4
        "col-xs-12"
        [ accordion "What is matrix.lgbt?" [ text "Because Matrix is a decentralized protocol, there are many home servers you can register with, and matrix.lgbt is just one of them. Matrix.lgbt aims to be a space for queer individuals and is owned and administered by Serra Allgood, a trans and non-binary sapphic. ", strong [] [ text "Please note that matrix.lgbt is not in any form directly affiliated with matrix.org." ] ]
        , accordion "What bridges does matrix.lgbt have?" [ text "Currently, bridges are set up for Telegram, WhatsApp, and Facebook Messenger." ]
        ]
    , col 4
        "col-xs-12"
        [ accordion "Who's on matrix.lgbt?" [ text "Matrix.lgbt is currently in its launch stage, so not many users live here. But it does federate with the overall Matrix-verse, meaning that joining gives you access to public rooms on any other server, including matrix.org and its many IRC bridges." ]
        , accordion "How can I join?" [ text "Right now, matrix.lgbt has semi-open registration. This means that in order to join, you need a registration token, which may be single use and will always have an expiration date. Consequently, it's a small server and it's mostly people who are close to Serra. If you're interested in joining, reach out to Serra by ", a [ href "mailto:root@allgood.mx" ] [ text "email." ] ]
        ]
    ]


accordion : String -> List (Html msg) -> Html msg
accordion header children =
    details [ class "accordion" ]
        [ summary [ class "accordion-header" ]
            [ h2 [] [ i [ class "icon icon-arrow-right mr-1" ] [], text header ]
            ]
        , div [ class "accordion-body" ] children
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
