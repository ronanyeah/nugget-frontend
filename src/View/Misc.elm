module View.Misc exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.View exposing (..)
import Img
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import Types exposing (..)


explorer val =
    --("https://solscan.io/tx/" ++ sig)
    "https://xray.helius.xyz/" ++ val ++ "/tx"


explorerAcc val =
    "https://xray.helius.xyz/" ++ val ++ "/wallet"


grey =
    rgb255 227 227 227


green =
    rgb255 0 255 0


shdw =
    Border.shadow
        { blur = 3, color = black, offset = ( 2, 2 ), size = 2 }


pair a b =
    [ boldText a
    , text b
    ]
        |> row [ spacing 10 ]


toId =
    Maybe.withDefault "SOL"


popLink xnft url label =
    if xnft then
        Input.button [ hover ]
            { onPress = Just <| OpenLink url
            , label = label
            }

    else
        newTabLink [ hover ]
            { url = url
            , label = label
            }


boldText =
    text
        >> el [ Font.size 24, font2 ]


headerText =
    text
        >> el [ Font.size 32, font2 ]


translate lang eng esp =
    case lang of
        Eng ->
            eng

        Esp ->
            esp


hover : Attribute msg
hover =
    Element.mouseOver [ fade ]


fade : Element.Attr a b
fade =
    Element.alpha 0.6


font1 =
    Font.family [ Font.typeface "Khula" ]


font2 =
    Font.family [ Font.typeface "Tilt Warp" ]


para xs =
    text >> List.singleton >> paragraph xs


btnAttr attr msg =
    btnToggle (Just msg) attr


btn msg =
    btnToggle (Just msg) []


btnToggle msg attrs elem =
    Input.button (attrs ++ [ hover ])
        { onPress = msg
        , label = elem
        }


black : Color
black =
    rgb255 0 0 0


white : Color
white =
    rgb255 255 255 255


lightGrey =
    rgb255 240 240 240


green2 =
    rgb255 54 222 54


viewIcon v n =
    image [ width <| px n ]
        { src = v
        , description = ""
        }


iconRow icn size space elem =
    [ icon icn size
    , elem
    ]
        |> row [ spacing space ]


icon : Icon msg -> Int -> Element msg
icon ic n =
    ic n Material.Icons.Types.Inherit
        |> Element.html
        |> el []


spinner : Int -> Element msg
spinner n =
    spinnerToggle n True


spinnerToggle : Int -> Bool -> Element msg
spinnerToggle n b =
    Img.notch n "black"
        |> el [ spin |> whenAttr b ]


spinny n b =
    icon Icons.refresh n
        |> el
            [ spin
                |> whenAttr b
            ]


spin : Attribute msg
spin =
    style "animation" "rotation 0.7s infinite linear"


fadeIn : Attribute msg
fadeIn =
    style "animation" "fadeIn 1.5s"


medText =
    text >> List.singleton >> paragraph [ Font.size 30, spacing 15 ]


smText =
    text >> List.singleton >> paragraph [ Font.size 25, spacing 15 ]


backing =
    el [ padding 10, Background.color white, width fill, Border.width 1 ]


withIcon icn elem =
    [ icon icn 25
    , elem
    ]
        |> row [ spacing 10 ]
