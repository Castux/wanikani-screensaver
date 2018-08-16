module KanjiScreen exposing (Model, Msg, init, view)

import Html
import Html.Attributes
import Window


type Msg
    = WindowResize Window.Size


type alias Model =
    Int


init kanjis ratio =
    0


view state =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "min-height", "100%" )
            , ( "background", "black" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ Html.span
            [ Html.Attributes.style
                [ ( "color", "white" )
                , ( "font-size", "20vmin" )
                , ( "font-family", "Noto Sans CJK JP" )
                , ( "margin", "auto" )
                ]
            ]
            [ Html.text "Gurido!" ]
        ]


subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize ]
