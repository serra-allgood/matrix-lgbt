module Nav exposing (nav)

import Html exposing (Html, a, header, section, text)
import Html.Attributes exposing (class, href, style)


nav : Html msg
nav =
    header [ class "navbar bg-primary p-sticky", style "top" "0", style "flex" "0 1 auto" ]
        [ section [ class "navbar-section" ]
            [ a [ href "/", class "navbar-brand mx-2 text-light" ] [ text "Matrix.lgbt" ]
            , a [ href "/registration", class "btn btn-link text-light" ] [ text "Register" ]
            ]
        , section [ class "navbar-section" ]
            [ a [ href "mailto:root@allgood.mx", class "btn btn-link text-light mx-2" ] [ text "Contact Serra" ] ]
        ]
