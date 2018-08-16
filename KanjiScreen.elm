module KanjiScreen exposing (Model, Msg, init, subscriptions, view)

import Html
import Html.Attributes
import KanjiData exposing (KanjiData)
import Layout
import Window


type Msg
    = WindowResize Window.Size


type alias Model =
    { aspect : Float
    , kanjis : List KanjiData
    }


init kanjis ratio =
    Model ratio kanjis


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
            [ Html.text "格子" ]
        ]


subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize ]
