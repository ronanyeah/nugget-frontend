module Main exposing (main)

import Browser
import Dict
import Maybe.Extra exposing (unwrap)
import Ports
import Task
import Time
import Types exposing (..)
import Update exposing (update)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { view = ViewHome
      , mobile = flags.screen.width < 1000
      , short = flags.screen.height < 900
      , fields =
            []
                |> Dict.fromList
      , balances = Dict.empty
      , active = Nothing
      , wallet = flags.xnft
      , connectInProgress = Nothing
      , qrInProgress = False
      , success = Nothing
      , qrCode = Nothing
      , warning = Nothing
      , bpkWarning = Nothing
      , isXnft = flags.xnft /= Nothing
      , walletOptions =
            flags.xnft
                |> Maybe.map (.meta >> List.singleton)
      , tokens = flags.tokens
      , language =
            flags.language
                |> Maybe.andThen
                    (\v ->
                        case v of
                            "eng" ->
                                Just Eng

                            "esp" ->
                                Just Esp

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault Eng
      , history = Dict.empty
      , verifyInProgress = False
      , currentToken = Nothing
      , fetchingHistory = Nothing
      , zone = Time.utc
      , tokenAdded = Nothing
      , profile = Nothing
      , solDomainInProgress = Nothing
      , backpackInProgress = False
      , shareEnabled = flags.shareEnabled
      }
    , Time.here
        |> Task.perform ZoneCb
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.buildTxCb BuildTxCb
        , Ports.connectCb ConnectCb
        , Ports.paymentCb PaymentCb
        , Ports.walletsCb WalletsCb
        , Ports.historyCb (Ok >> HistoryCb)
        , Ports.historyErr (Err >> HistoryCb)
        , Ports.tokenCb (Ok >> TokenCb)
        , Ports.tokenErr (Err >> TokenCb)
        , Ports.solDomainCb (unwrap (Err ()) Ok >> VerifySolCb)
        ]
