module View exposing (view)

import Date
import DateTime exposing (DateTime)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Helpers.View exposing (..)
import Html exposing (Html)
import Html.Attributes
import Img
import List.Extra
import Material.Icons as Icons
import Maybe.Extra exposing (unwrap)
import Misc exposing (..)
import Time exposing (Zone)
import Types exposing (..)
import View.Connect
import View.Misc exposing (..)


view : Model -> Html Msg
view model =
    (if model.mobile then
        viewMobile

     else
        viewApp
    )
        model
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }

                --|> (if isMobile then
                --(::) Element.noHover
                --else
                --identity
                --)
                ]
            }
            [ width fill
            , height fill
            , style "-webkit-tap-highlight-color" "transparent"
            , style "tap-highlight-color" "transparent"
            , Maybe.map2 (viewQR model) model.qrCode model.wallet
                |> whenJust identity
                |> inFront
            , Background.image "/bg.jpg"
            , font1
            ]


viewMobile model =
    [ [ [ [ image [ width <| px 35 ]
                { src = "/nugget.png"
                , description = ""
                }
          , text "nugget"
                |> el
                    [ Font.size 32
                    , font2
                    ]
          ]
            |> row [ spacing 10, fadeIn ]
            |> (\elem ->
                    Input.button []
                        { onPress = Just <| SetView ViewHome
                        , label = elem
                        }
               )
        , [ icon Icons.home 30
                |> btn (SetView ViewHome)
          , icon Icons.settings 30
                |> btn (SetView ViewSettings)
          ]
            |> row [ spacing 10 ]
            |> when model.isXnft
        ]
            |> row [ spacing 20 ]
      , model.wallet
            |> unwrap
                (viewNav model)
                (\wallet ->
                    [ image [ height <| px 18 ]
                        { src = wallet.meta.icon
                        , description = ""
                        }
                    , wallet.label
                        |> Maybe.withDefault (String.left 10 wallet.address ++ "...")
                        |> text
                        |> el [ Font.size 17, moveDown 2 ]
                    ]
                        |> row
                            [ Border.width 1
                            , spacing 10
                            , Border.rounded 7
                            , Background.color white
                            , paddingXY 15 10
                            ]
                        |> btn (SetView ViewSettings)
                        |> when (not model.isXnft)
                )
      ]
        |> row [ width fill, spaceEvenly ]
    , case model.view of
        ViewInput ->
            model.active
                |> unwrap (text "oops") (viewInput model)

        ViewWallets ->
            View.Connect.view model

        ViewSettings ->
            viewSettings model

        ViewHistory ->
            viewHistory model

        ViewNewToken ->
            viewNewToken model

        ViewHome ->
            [ [ title1 model.language
                    |> smText
              , title2 model.language
                    |> smText
              ]
                |> column [ spacing 35, cappedWidth 450, Font.center, alignTop ]
            , tokens
                ++ model.tokens
                |> List.map
                    (viewPill SetActive)
                |> (\xs -> xs ++ [ newTokenButton model.language 10 140 13 ])
                |> List.Extra.greedyGroupsOf 2
                |> List.map
                    (wrappedRow [ spacing 30 ])
                |> column [ spacing 20, centerX ]
            ]
                |> column [ spacing 40, fadeIn ]
                |> (\elem ->
                        Element.Keyed.el
                            [ centerX ]
                            ( "tokens", elem )
                   )
    ]
        |> column
            [ height fill
            , cappedWidth 1400
            , spacing 30
            , centerX
            , paddingXY 10 20
            , centerX
            ]


viewConnect lang mobile =
    [ image [ height <| px (switch mobile 18 22) ]
        { src = "/tokens/solana.png"
        , description = ""
        }
    , translate lang "Connect Wallet" "Conectar Wallet"
        |> text
        |> el [ Font.size 17, moveDown 2 ]
    ]
        |> row
            [ Border.width 1
            , spacing (switch mobile 10 15)
            , Border.rounded 7
            , Background.color white
            , switch mobile (paddingXY 15 10) (paddingXY 20 13)
            ]
        |> btn GetWallets


viewApp model =
    [ [ [ image [ width <| px 50 ]
            { src = "/nugget.png"
            , description = ""
            }
        , [ text "nugget"
                |> el
                    [ Font.size 45
                    , centerY
                    , font2
                    ]
          , text "pay"
                |> el
                    [ Font.size 38
                    , moveDown 9
                    ]
          ]
            |> row [ spacing 13, moveUp 3 ]
        ]
            |> row
                [ spacing 15
                , fadeIn
                ]
            |> btn (SetView ViewHome)
      , model.wallet
            |> unwrap (viewNav model)
                (\wallet ->
                    [ image [ height <| px 22 ]
                        { src = wallet.meta.icon
                        , description = ""
                        }
                    , wallet.label
                        |> Maybe.withDefault (String.left 10 wallet.address ++ "...")
                        |> text
                    ]
                        |> row
                            [ Border.width 1
                            , spacing 15
                            , Border.rounded 7
                            , Background.color white
                            , paddingXY 20 13
                            ]
                        |> btn (SetView ViewSettings)
                )
      ]
        |> row [ width fill, spaceEvenly ]
    , case model.view of
        ViewInput ->
            model.active
                |> unwrap (text "oops") (viewInput model)

        ViewWallets ->
            View.Connect.view model

        ViewSettings ->
            viewSettings model

        ViewHistory ->
            viewHistory model

        ViewHome ->
            viewTokens model

        ViewNewToken ->
            viewNewToken model
    , viewTwitter model.isXnft
        |> el [ centerX, alignBottom ]
    ]
        |> column
            [ height fill
            , cappedWidth 1400
            , spacing (switch model.short 40 80)
            , centerX
            , padding (switch model.mobile 20 40)
            ]


viewNewToken model =
    [ boldText (translate model.language "Add New SPL token" "Añadir Nuevo SPL token")
    , [ Input.text [ width fill ]
            { label = Input.labelHidden ""
            , onChange = SetText "token"
            , placeholder = Just <| Input.placeholder [] <| text "SPL token mint address"
            , text = get "token" model.fields
            }
      , [ model.warning
            |> whenJust
                (text
                    >> List.singleton
                    >> paragraph
                        [ alignTop
                        , Font.color <| rgb255 245 0 0
                        , Font.italic
                        , Font.size 17
                        ]
                )
        , "+"
            |> text
            |> el [ Font.size 55 ]
            |> btnToggle
                (if model.verifyInProgress then
                    Nothing

                 else
                    Just VerifyToken
                )
                []
            |> el [ alignRight ]
        ]
            |> row [ width fill ]
      ]
        |> column [ spacing 10, width fill ]
    , model.tokenAdded
        |> whenJust
            (\tk ->
                [ smText (translate model.language "Token Added!" "Agregado el token!")
                    |> el [ Font.italic ]
                , [ [ image [ height <| px 30 ]
                        { src = tk.img
                        , description = ""
                        }
                    , splLink model.isXnft tk
                    ]
                        |> row
                            [ spacing 10
                            ]
                  , [ pair "Symbol" tk.symbol
                    , pair "Decimals" <| String.fromInt tk.decimals
                    ]
                        |> column [ spacing 10 ]
                  ]
                    |> column [ spacing 25 ]
                , "📸 "
                    ++ translate model.language "Generate QR code" "Generar codigo QR"
                    |> text
                    |> btn (SetActive tk)
                    |> el [ centerX, Font.underline, Font.italic ]
                ]
                    |> column
                        [ Border.width 2
                        , Background.color grey
                        , spacing 20
                        , paddingXY 20 20
                        , width fill
                        ]
            )
    ]
        |> column
            [ centerX
            , spacing 30
            , cappedWidth 450
            ]


viewInput : Model -> Token -> Element Msg
viewInput model val =
    [ paragraph [ Font.size 24, spacing 30, Font.center ]
        [ translate model.language "How much " "¿Cuánto "
            |> text
        , el [ width <| px 5 ] none
        , viewToken val
        , el [ width <| px 5 ] none
        , translate model.language " do you want to receive?" " quieres recibir?"
            |> text
        ]
    , [ Input.text [ width <| px 150, Font.alignRight ]
            { label = Input.labelHidden ""
            , onChange = SetText "nums"
            , placeholder = Just <| Input.placeholder [] <| text "0.00"
            , text = get "nums" model.fields
            }
      , splLink model.isXnft val
      ]
        |> row [ spacing 10, centerX ]
    , [ viewIcon val.img 32
      , translate model.language "Create QR code" "Crear Codigo QR"
            |> text
            |> el [ Font.size 22, moveDown 3 ]
      ]
        |> row
            [ spacing 25
            , Background.color black
            , Font.color white
            , paddingXY 50 16
            , Border.rounded 8
            , Font.bold
            , Img.notch 22 "white"
                |> el [ spin ]
                |> el
                    [ centerY
                    , paddingXY 8 0
                    , alignRight
                    , Font.color white
                    , moveUp 3
                    ]
                |> inFront
                |> whenAttr model.qrInProgress
            ]
        |> btnToggle
            (if model.qrInProgress then
                Nothing

             else
                Just SubmitAmount
            )
            []
        |> el [ centerX ]
    , paymentHistoryLink model.language val
    ]
        |> column [ spacing 40, fadeIn ]
        |> (\elem ->
                Element.Keyed.el
                    [ centerX ]
                    ( "input", elem )
           )


viewTokens : Model -> Element Msg
viewTokens model =
    [ [ title1 model.language
            |> medText
      , title2 model.language
            |> medText
            |> el [ centerX ]
      ]
        |> column [ spacing 35, cappedWidth 450, Font.center, alignTop ]
    , tokens
        ++ model.tokens
        |> List.map
            (\tk ->
                Input.button
                    [ hover
                    , paddingXY 0 15
                    , width <| px 180
                    , Border.rounded 7
                    , Background.color white
                    , shdw
                    ]
                    { onPress = Just <| SetActive tk
                    , label =
                        [ image [ height <| px 30 ]
                            { src = tk.img
                            , description = ""
                            }
                        , tk.name
                            |> String.left 12
                            |> text
                            |> el [ cappedWidth 420, alignRight ]
                        ]
                            |> row
                                [ spacing 20
                                , centerX
                                ]
                    }
            )
        |> (\xs ->
                xs
                    ++ [ newTokenButton model.language 15 180 17
                       ]
           )
        |> List.Extra.greedyGroupsOf 3
        |> List.map
            (row [ spacing 60 ])
        |> column
            [ spacing 20
            , centerX
            ]
    ]
        |> row [ spacing 100, fadeIn ]
        |> (\elem ->
                Element.Keyed.el
                    [ centerX ]
                    ( "tokens", elem )
           )


viewAmt amt tk =
    [ viewIcon tk.img 27
    , [ amt
            |> String.toFloat
            |> whenJust (formatFloat >> text)
      , tk.symbol
            |> text
            |> el [ Font.bold, Font.size 25 ]
      ]
        |> row [ moveDown 3, spacing 8, Font.size 30 ]
    ]
        |> row [ spacing 8 ]


formatFloat =
    FormatNumber.format
        { usLocale | decimals = FormatNumber.Locales.Exact 2 }


formatDeci n dec =
    (String.toInt n
        |> Maybe.withDefault 0
        |> toFloat
    )
        / 10
        ^ toFloat dec


viewToken tk =
    Input.button
        [ hover
        , paddingXY 10 5
        , Border.rounded 7
        , Background.color white
        , Border.width 1
        ]
        { onPress = Just <| SetView ViewHome
        , label =
            text tk.name
                |> el
                    [ Font.size 20
                    , image [ height <| px 20, alignLeft, moveDown 4 ]
                        { src = tk.img
                        , description = ""
                        }
                        |> inFront
                    , paddingEach
                        { top = 0
                        , bottom = 0
                        , left = 30
                        , right = 0
                        }
                    ]
        }


viewQR model qr wallet =
    let
        addr =
            wallet.label
                |> unwrap
                    (trimAddr wallet.address
                        |> text
                        |> el [ Font.bold ]
                    )
                    (\v ->
                        [ viewIcon wallet.meta.icon 20
                        , text v
                            |> el [ Font.bold ]
                        ]
                            |> row [ spacing 5 ]
                    )

        sharePill icn col txt msg =
            [ icon icn 17
                |> el [ Font.color col ]
            , text txt
            ]
                |> row
                    [ spacing 10
                    , Font.italic
                    , Background.color lightGrey
                    , paddingXY 20 10
                    , Border.rounded 20
                    , Border.width 1
                    , Font.size 17
                    ]
                |> btn msg
    in
    [ model.success
        |> unwrap
            ([ image
                [ cappedHeight 400
                , cappedWidth 400
                ]
                { src = qr
                , description = ""
                }
             , [ [ addr
                 , translate model.language "is requesting" "está solicitando"
                    |> text
                 ]
                    |> row [ spacing 10 ]
               , model.active
                    |> whenJust (viewAmt (get "nums" model.fields))
                    |> el [ centerX ]
               ]
                |> column [ spacing 15, centerX ]
             , sharePill Icons.content_copy (rgb255 165 145 0) "Copy QR" (CopyQR qr)
                |> (\copyBtn ->
                        if model.shareEnabled then
                            [ copyBtn
                            , sharePill Icons.share (rgb255 0 163 0) "Share QR" (ShareQR qr)
                            ]
                                |> row [ spacing 20, centerX ]

                        else
                            copyBtn
                                |> el [ centerX ]
                   )
             ]
                |> column [ spacing 20 ]
            )
            (\sig ->
                [ icon Icons.task_alt 175
                    |> el
                        [ fadeIn
                        , Font.color green2
                        ]
                , translate model.language "Success!" "Éxito!"
                    |> text
                    |> el [ Font.bold, centerX, Font.size 30 ]
                , "🔎  "
                    ++ translate model.language "View Transaction" "Ver transacción"
                    |> text
                    |> popLink model.isXnft (explorer sig)
                    |> el [ Font.underline, centerX ]
                ]
                    |> column
                        [ spacing 30
                        , centerY
                        , centerX
                        ]
                    |> el
                        [ if model.mobile then
                            width fill

                          else
                            width <| px 400
                        , height fill
                        ]
            )
    ]
        |> column
            [ spacing 20
            , height fill
            , width fill
                |> whenAttr model.mobile
            ]
        |> el
            [ padding 30
            , Background.color white
            , Border.rounded 30
            , width fill
                |> whenAttr model.mobile
            , Border.width 2
            , Input.button
                [ alignRight
                , alignTop
                , padding 6
                ]
                { onPress = Just <| BuildTxCb Nothing
                , label = icon Icons.close 40
                }
                |> inFront
            ]
        |> el
            [ padding 15
            , centerX
            , centerY
            , width fill
                |> whenAttr model.mobile
            ]
        |> el
            [ width fill
            , height fill
            , Background.color <| rgba255 0 0 0 0.1
            , style "animation" "fadeIn 0.5s"
            ]


viewHistory model =
    let
        fetching =
            model.fetchingHistory /= Nothing
    in
    [ [ [ boldText (translate model.language "Received Payments" "Pagos recibidos")
        , para [] (translate model.language "View all payments received through Nugget Pay." "Ver todos los pagos recibidos a través de Nugget Pay.")
        ]
            |> column [ spacing 10, width fill ]
      , spinny 35 fetching
            |> (if fetching then
                    identity

                else
                    btn RefreshHistory
               )
            |> when (model.currentToken /= Nothing)
      ]
        |> row [ width fill, spaceEvenly ]
    , tokens
        ++ model.tokens
        |> List.map
            (\tk ->
                [ [ [ image [ height <| px 20 ]
                        { src = tk.img
                        , description = ""
                        }
                    , text tk.name
                        |> el [ moveDown 2 ]
                    , model.balances
                        |> Dict.get (toId tk.address)
                        |> whenJust
                            (\val ->
                                [ icon Icons.account_balance_wallet 20
                                , text <| formatFloat val
                                ]
                                    |> row
                                        [ Background.color grey
                                        , padding 5
                                        , Border.rounded 5
                                        , spacing 5
                                        ]
                            )
                    ]
                        |> row
                            [ spacing 20
                            ]
                  , icon
                        (if model.currentToken == Just tk.address then
                            Icons.expand_less

                         else
                            Icons.expand_more
                        )
                        35
                  ]
                    |> row [ width fill, spaceEvenly ]
                    |> btnAttr [ width fill ] (SetHistory tk.address)
                , getHistory model.history model.currentToken
                    |> List.sortBy .timestamp
                    |> List.reverse
                    |> (\xs ->
                            let
                                isBeingFetched =
                                    model.fetchingHistory == Just tk.address
                            in
                            if List.isEmpty xs then
                                if isBeingFetched then
                                    spinny 35 True
                                        |> el [ centerX ]

                                else if model.fetchingHistory == Nothing then
                                    "🔍  "
                                        ++ " "
                                        ++ translate model.language "Fetch history" "Recuperar el historial"
                                        |> text
                                        |> el []
                                        |> btn RefreshHistory
                                        |> el [ centerX ]

                                else
                                    text "..."
                                        |> el [ centerX ]

                            else
                                [ spinny 30 isBeingFetched
                                    |> btnToggle
                                        (if model.fetchingHistory == Nothing then
                                            Just RefreshHistory

                                         else
                                            Nothing
                                        )
                                        []
                                    |> el [ alignRight ]
                                , xs
                                    |> List.map
                                        (\x ->
                                            let
                                                lft =
                                                    [ popLink model.isXnft
                                                        (explorer x.signature)
                                                        (trimAddr x.signature
                                                            |> text
                                                            |> el [ hover, Font.underline ]
                                                        )
                                                    , [ boldText (translate model.language "Sent by" "Enviado por")
                                                      , [ icon Icons.person 20
                                                        , text (trimAddr x.sender)
                                                            |> el [ Font.size 15 ]
                                                        ]
                                                            |> row
                                                                [ spacing 5
                                                                , Background.color white
                                                                , Border.rounded 30
                                                                , paddingXY 10 8
                                                                , Border.width 1
                                                                ]
                                                      ]
                                                        |> row [ spacing 10, width fill ]
                                                    ]
                                                        |> column
                                                            [ spaceEvenly
                                                            , alignTop
                                                            , height fill
                                                            , width fill
                                                            ]

                                                rgt =
                                                    [ x.timestamp
                                                        |> Time.millisToPosix
                                                        |> DateTime.fromPosix
                                                        |> formatZoned model.zone
                                                        |> text
                                                    , [ "+"
                                                            ++ String.fromFloat (formatDeci x.amount tk.decimals)
                                                            |> text
                                                            |> el
                                                                [ Font.size 50
                                                                , Font.color green
                                                                ]
                                                      , tk.symbol
                                                            |> text
                                                            |> el
                                                                [ Font.size 30
                                                                , Font.bold
                                                                , font2
                                                                ]
                                                      ]
                                                        |> row
                                                            [ Font.size 25
                                                            ]
                                                    ]
                                                        |> column [ spacing 20 ]
                                            in
                                            if model.mobile then
                                                [ lft
                                                , rgt
                                                ]
                                                    |> column
                                                        [ spacing 40
                                                        , width fill
                                                        , Background.color grey
                                                        , paddingXY 10 10
                                                        , Border.rounded 10
                                                        ]

                                            else
                                                [ lft
                                                , rgt
                                                ]
                                                    |> row
                                                        [ spaceEvenly
                                                        , width fill
                                                        , Background.color grey
                                                        , paddingXY 10 10
                                                        , Border.rounded 10
                                                        ]
                                        )
                                    |> column
                                        [ spacing 20
                                        , height fill
                                        , width fill
                                        ]
                                ]
                                    |> column
                                        [ height fill
                                        , width fill
                                        ]
                       )
                    |> when (model.currentToken == Just tk.address)
                ]
                    |> column
                        [ width fill
                        , spacing
                            (if model.mobile then
                                10

                             else
                                30
                            )
                        , Background.color white
                        , Border.width 2
                        , Border.rounded 10
                        , padding 20
                        ]
            )
        |> column [ spacing 20, width fill, height fill, scrollbarY ]
    ]
        |> column
            [ centerX
            , spacing (switch model.short 20 40)
            , cappedWidth 570
            , height fill
            ]


viewSettings model =
    [ [ [ icon Icons.qr_code_2 25
        , translate model.language "Create QR code" "Crear Codigo QR"
            |> text
        ]
            |> row
                [ Background.color white
                , paddingXY 20 10
                , Border.width 1
                , Border.rounded 5
                , spacing 10
                ]
            |> btn (SetView ViewHome)
      , [ icon Icons.add 25
        , translate model.language "Add SPL token" "Agregar SPL"
            |> text
        ]
            |> row
                [ Background.color white
                , paddingXY 20 10
                , Border.width 1
                , Border.rounded 5
                , spacing 10
                ]
            |> btn (SetView ViewNewToken)
      , [ icon Icons.format_list_bulleted 25
        , translate model.language "Payment History" "Historial de pagos"
            |> text
        ]
            |> row
                --[ paddingXY 20 10
                --, Background.color <| rgb255 0 0 255
                --, Border.rounded 5
                --, Font.color white
                --, Font.bold
                --, Border.width 1
                --, Border.color black
                --]
                [ Background.color white
                , paddingXY 20 10
                , Border.width 1
                , Border.rounded 5
                , spacing 10
                ]
            |> btn (GotoHistory Nothing)
      ]
        |> wrappedRow [ width fill, spacing (switch model.mobile 10 30) ]
    , [ [ boldText (translate model.language "Connected" "Conectado")
        , model.wallet
            |> whenJust
                (\val ->
                    [ val.label
                        |> whenJust
                            (\v ->
                                v
                                    ++ " |"
                                    |> text
                                    |> el [ Font.bold ]
                            )
                    , trimAddr val.address
                        |> text
                        |> el [ Font.underline ]
                    , icon Icons.launch 23
                        |> el [ moveUp 4 ]
                    ]
                        |> row [ spacing 10 ]
                        |> popLink model.isXnft
                            (explorerAcc val.address)
                )

        --, trimAddr model
        ]
            |> column [ spacing 20 ]
      , translate model.language "Language" "Idioma"
            |> boldText
      , [ Eng, Esp ]
            |> List.map
                (\l ->
                    (case l of
                        Eng ->
                            "🇬🇧  English"

                        Esp ->
                            "🇪🇸  Español"
                    )
                        |> text
                        |> el
                            [ Background.color white
                            , paddingXY 20 10
                            , Border.rounded 10
                            , Border.width 2
                                |> whenAttr (l == model.language)
                            ]
                        |> btn (SetLang l)
                )
            |> row [ spacing 20 ]
      ]
        |> column [ spacing 20 ]
    , translate model.language "Disconnect Wallet" "Desconectar Wallet"
        |> text
        |> el
            [ paddingXY 20 10
            , Background.color <| rgb255 255 0 0
            , Border.rounded 5
            , Font.color white
            , Font.bold
            , Border.width 1
            , Border.color black
            ]
        |> btn Disconnect
        |> when (not model.isXnft)
    , viewTwitter model.isXnft
        |> el [ centerX ]
        |> when model.mobile
    ]
        |> column
            [ spacing 40
            , width fill
            , height fill
            ]
        |> (\elem ->
                Element.Keyed.el
                    [ centerX, cappedWidth 450, fadeIn, height fill, scrollbarY ]
                    ( "settings", elem )
           )


title1 lang =
    translate lang
        "Request instant payment by QR code ⚡"
        "Solicita un pago instantáneo por código QR ⚡"


title2 lang =
    translate lang
        "How do you want to be paid?"
        "¿Cómo quieres que te paguen?"


viewTwitter xnft =
    popLink
        xnft
        "https://twitter.com/nugget_pay"
        ([ Img.twitter 30, text "@nugget_pay" ]
            |> row
                [ Font.size 30
                , spacing 10
                ]
        )


newTokenButton lang yPad wd fnt =
    Input.button
        [ hover
        , paddingXY 0 yPad
        , width <| px wd
        , Border.rounded 7
        , Background.color white
        , shdw
        ]
        { onPress = Just (SetView ViewNewToken)
        , label =
            [ icon Icons.add (fnt + 10)
            , translate lang "Add SPL token" "Agregar SPL"
                |> text
                |> el [ Font.size fnt, moveDown 2 ]
            ]
                |> row
                    [ spacing 10
                    , centerX
                    ]
        }


viewPill msg tk =
    Input.button
        [ hover
        , paddingXY 0 10
        , width <| px 140
        , Border.rounded 7
        , Background.color white
        , shdw
        ]
        { onPress = Just <| msg tk
        , label =
            [ image [ height <| px 20 ]
                { src = tk.img
                , description = ""
                }
            , tk.name
                |> String.left 10
                |> text
                |> el [ moveDown 2 ]
            ]
                |> row
                    [ spacing 10
                    , centerX
                    ]
        }


formatDateTime : DateTime -> String
formatDateTime d =
    [ padNum (DateTime.getHours d) ++ ":" ++ padNum (DateTime.getMinutes d)
    , [ d
            |> DateTime.getDay
            |> padNum
      , d
            |> DateTime.getMonth
            |> Date.monthToNumber
            |> padNum
      , d |> DateTime.getYear |> String.fromInt
      ]
        |> String.join "-"
    ]
        |> String.join " "


formatZoned : Zone -> DateTime -> String
formatZoned z =
    shiftDateTime z >> formatDateTime


shiftDateTime : Zone -> DateTime -> DateTime
shiftDateTime zone d =
    let
        posix =
            DateTime.toPosix d

        offset =
            DateTime.getTimezoneOffset zone posix
    in
    (Time.posixToMillis posix + offset)
        |> Time.millisToPosix
        |> DateTime.fromPosix


padNum : Int -> String
padNum =
    String.fromInt >> String.padLeft 2 '0'


getHistory dct tk =
    tk
        |> unwrap []
            (\curr ->
                dct
                    |> Dict.get (toId curr)
                    |> Maybe.withDefault []
            )


splLink xnft val =
    [ text val.symbol
    , icon Icons.launch 23
        |> el [ moveUp 4 ]
    ]
        |> row [ Font.bold, Font.size 23, spacing 7 ]
        |> popLink xnft
            (val.address
                |> unwrap "https://explorer.solana.com/supply"
                    (\addr -> "https://solscan.io/token/" ++ addr)
            )


paymentHistoryLink lang val =
    "📒  "
        ++ translate lang
            ("View " ++ val.symbol ++ " payment history")
            ("Ver historial de pago de " ++ val.symbol)
        |> text
        |> btn (GotoHistory (Just val.address))
        |> el [ centerX, Font.underline, Font.italic ]


viewNav model =
    [ viewFlags model
        |> when (not model.mobile)
    , viewConnect model.language model.mobile
    ]
        |> row [ spacing 10 ]
        |> when (model.view /= ViewWallets)


viewFlags model =
    [ Eng, Esp ]
        |> List.map
            (\l ->
                (case l of
                    Eng ->
                        "🇬🇧"

                    Esp ->
                        "🇪🇸"
                )
                    |> text
                    |> el
                        [ Background.color white
                        , Font.size 15
                        , paddingXY 7 7
                        , Border.rounded 5
                        , Border.width 1
                            |> whenAttr (l == model.language)
                        ]
                    |> btn (SetLang l)
            )
        |> row [ spacing 10 ]
