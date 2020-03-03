module Layout exposing (col, layout)

import Browser
import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href, style)
import Nav exposing (nav)


layout : String -> List (Html msg) -> Browser.Document msg
layout title html =
    { title = "Matrix.lgbt - " ++ title
    , body =
        [ nav
        , div
            [ class "hero"
            , style "background-image" "url('pride-hero-bg.jpg')"
            , style "background-repeat" "no-repeat"
            , style "background-size" "100% 100%"
            ]
            [ div [ class "hero-body text-center p-centered s-rounded d-inline-block", style "background-color" "#8F3183", style "color" "#EFCFF5" ]
                [ h1 [] [ text "Matrix.lgbt" ]
                , p []
                    [ text "A home for LGBTQIA+ individuals in the "
                    , a [ href "https://matrix.org" ] [ text "Matrix-verse" ]
                    ]
                ]
            ]
        , div
            [ class "container pt-2"
            , style "background-color" "#140E53"
            , style "color" "#5A9775"
            , style "min-height" "100vh"
            ]
            [ div [ class "columns" ] html ]
        ]
    }


col : Int -> String -> List (Html msg) -> Html msg
col colNum class_ children =
    div [ class ("column col-" ++ String.fromInt colNum), class class_ ] children
