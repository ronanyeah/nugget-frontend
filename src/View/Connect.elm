module View.Connect exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Helpers.View exposing (..)
import Material.Icons as Icons
import Maybe.Extra exposing (unwrap)
import Misc exposing (..)
import Types exposing (..)
import View.Misc exposing (..)


view model =
    [ [ translate model.language
            "Connection Options"
            "Opciones de conexión"
            |> headerText
      , translate model.language
            "You will be able to create QR codes and request instant payments."
            "Podrás crear códigos QR y solicitar pagos instantáneos."
            |> para []
      ]
        |> column [ spacing 10 ]

    --|> when (not <| List.isEmpty ws)
    , model.walletOptions
        |> unwrap (solDomainEntry model)
            (\ws ->
                [ solDomainEntry model
                    |> backing

                --, smText (translate model.language "Or" "O")
                --|> el [ centerX ]
                --|> when (not <| List.isEmpty ws)
                , walletSelect model ws
                    |> backing

                --|> when (not <| List.isEmpty ws)
                ]
                    |> column
                        [ spacing 30
                        , width fill
                        , height fill
                        , scrollbarY
                        ]
            )
    ]
        |> column
            [ spacing 30
            , width fill
            , height fill
            ]
        |> (\elem ->
                Element.Keyed.el
                    [ centerX, cappedWidth 500, fadeIn, height fill ]
                    ( "connect", elem )
           )


walletSelect model ws =
    [ [ icon Icons.wallet 20
            |> el [ Font.color <| rgb255 165 42 42 ]
      , translate model.language
            "Connect a Wallet"
            "Conectar una Wallet"
            |> boldText
      ]
        |> row [ spacing 5 ]
    , ws
        |> List.map
            (\w ->
                Input.button
                    [ hover
                        |> whenAttr (model.connectInProgress == Nothing)
                    , fade
                        |> whenAttr (model.connectInProgress /= Nothing && model.connectInProgress /= Just w.name)
                    , Font.bold
                        |> whenAttr (model.connectInProgress == Just w.name)
                    , spinner 20
                        |> el [ centerY, paddingXY 10 0 ]
                        |> onRight
                        |> whenAttr (model.connectInProgress == Just w.name)
                    ]
                    { onPress =
                        if model.connectInProgress == Nothing then
                            Just <| Connect w.name

                        else
                            Nothing
                    , label =
                        [ image [ height <| px 20 ] { src = w.icon, description = "" }
                        , text w.name
                        ]
                            |> row [ spacing 10 ]
                    }
            )
        |> (\xs ->
                if List.isEmpty xs then
                    [ [ translate model.language "No Solana Wallets have been detected. " "No se han detectado Wallets de Solana. "
                            |> smText
                      , translate model.language
                            "You can learn more about installing one "
                            "Puede obtener más información sobre cómo instalar una Wallet "
                            |> smText
                      , popLink model.isXnft
                            "https://solana.com/ecosystem/explore?categories=wallet"
                            (translate model.language "here" "aquí"
                                |> smText
                                |> el [ Font.bold, Font.underline ]
                            )
                      , smText "."

                      --, medText " found!"
                      --, smText " about choosing one."
                      ]
                        |> paragraph [ Font.center ]
                    ]

                else
                    xs
           )
        |> column
            [ spacing 20
            , width fill
            , Background.color <| rgb255 228 228 228
            , padding 15
            , Border.width 1
            ]
    ]
        |> column [ spacing 10, width fill ]


solDomainEntry model =
    let
        inProg =
            model.solDomainInProgress /= Nothing
    in
    [ Input.text
        [ width fill
        , onKeydown "Enter" VerifySol
            |> whenAttr (not inProg)
        ]
        { label =
            [ [ [ icon Icons.language 20
                    |> el [ Font.color <| rgb255 0 0 255 ]
                , boldText (translate model.language "Use an SNS/ANS domain" "Usa tu dominio .sol")
                ]
                    |> row [ spacing 5 ]
              , text ".sol .abc .bonk .poor"
                    |> el [ Font.italic ]
              ]
                |> column [ spacing 10 ]
            , model.warning
                --Just "There was a problem!"
                |> whenJust
                    (text
                        >> List.singleton
                        >> paragraph
                            [ Font.color <| rgb255 245 0 0
                            , Font.italic
                            , Font.size 17
                            , alignBottom
                            , Font.alignRight
                            ]
                    )
            ]
                |> row [ width fill, spaceEvenly ]
                |> Input.labelAbove [ width fill ]
        , onChange = SetDomainText
        , placeholder = Just <| Input.placeholder [] <| text "moonbags.sol"
        , text = get "name" model.fields
        }
    , [ [ ".sol"
            |> text
            |> el [ Font.size 35 ]
            |> btnToggle
                (if inProg then
                    Nothing

                 else
                    Just VerifySol
                )
                [ centerX ]
            |> when False
        , "Backpack"
            |> text
            |> el [ Font.size 35 ]
            |> btnToggle
                (if model.verifyInProgress then
                    Nothing

                 else
                    Just VerifyBackpack
                )
                []
            |> when False
        ]
            |> row
                [ --alignRight
                  centerX
                , spacing 20
                ]
      ]
        |> row [ width fill ]
    , model.profile
        |> unwrap
            ([ [ image [ height <| px (switch model.mobile 18 22) ]
                    { src = "/tokens/solana.png"
                    , description = ""
                    }
               , translate model.language "Search" "Buscar"
                    |> text
                    |> el [ Font.size 17, moveDown 2 ]
               ]
                |> row
                    [ centerX
                    , spacing (switch model.mobile 10 15)
                    , spinner 20
                        |> el [ centerY, paddingXY 10 0 ]
                        |> onRight
                        |> whenAttr (model.solDomainInProgress /= Nothing)
                    ]
                |> el
                    [ Border.width 1
                    , Border.rounded 7
                    , Background.color white
                    , switch model.mobile (paddingXY 15 10) (paddingXY 20 13)
                    , width fill
                    ]
                |> btnToggle
                    (if inProg then
                        Nothing

                     else
                        Just VerifySol
                    )
                    [ width fill ]
             , translate model.language "Learn more about .sol domains" "¿Qué es un dominio .sol?"
                |> text
                |> el [ Font.underline, Font.italic, Font.size 17 ]
                |> popLink model.isXnft "https://naming.bonfida.org/"
                |> el
                    [ alignRight
                    , paddingEach
                        { top = 10
                        , bottom = 0
                        , left = 0
                        , right = 0
                        }
                    ]
             ]
                |> column [ width fill, spacing 5 ]
            )
            (\w ->
                [ [ [ icon Icons.saved_search 20
                        |> el [ Font.color green ]
                    , translate model.language "Wallet found" "Wallet encontrada"
                        |> Just
                        --, w.label
                        |> whenJust
                            (text
                                >> el [ Font.bold ]
                            )
                    ]
                        |> row [ spacing 5 ]
                  , [ w.label
                        |> whenJust
                            (text
                                >> el [ Font.bold ]
                            )
                    , "| "
                        ++ (w.address
                                |> trimAddr
                           )
                        |> text
                    , icon Icons.launch 23
                        |> el [ moveUp 4 ]
                    ]
                        |> row [ spacing 5 ]
                        |> popLink model.isXnft (explorerAcc w.address)
                  ]
                    |> wrappedRow
                        [ width fill
                        , spaceEvenly
                        , Background.color white
                        , paddingXY 20 10
                        , Border.width 1
                        ]
                , [ translate model.language "Cancel" "Cancelar"
                        |> text
                        |> el [ Font.underline ]
                        |> btn CancelConnect
                  , [ icon Icons.mode_standby 20
                        |> when False
                    , translate model.language "Select" "Seleccionar"
                        |> text
                        |> el [ moveDown 3 ]
                    ]
                        |> row
                            [ Background.color white
                            , spacing 10
                            , Border.width 1
                            , paddingXY 20 10
                            , Border.rounded 5
                            ]
                        |> btn ConnectSelect
                  ]
                    |> row [ alignRight, spacing 20 ]
                ]
                    |> column [ spacing 10, width fill, fadeIn ]
            )
    ]
        |> column [ width fill, spacing 10 ]
