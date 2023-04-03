module Update exposing (update)

import Dict
import Helpers.Http exposing (jsonResolver)
import Http
import Img
import Json.Decode as JD
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc
import Ports
import Result.Extra exposing (unpack)
import Task
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ensureWallet fn =
            model.wallet
                |> unwrap ( model, Ports.log "Wallet missing" ) fn
    in
    case msg of
        GotoHistory val ->
            ensureWallet
                (\w ->
                    let
                        shouldFetch =
                            model.fetchingHistory
                                == Nothing
                                && (val
                                        |> unwrap
                                            False
                                            (\vv ->
                                                Dict.get
                                                    (vv
                                                        |> Maybe.withDefault "SOL"
                                                    )
                                                    model.history
                                                    == Nothing
                                            )
                                   )
                    in
                    ( { model
                        | view = ViewHistory
                        , currentToken =
                            val
                                |> unwrap
                                    model.currentToken
                                    Just
                        , fetchingHistory =
                            if shouldFetch then
                                val

                            else
                                Nothing
                      }
                    , val
                        |> unwrap Cmd.none
                            (\tk ->
                                if shouldFetch then
                                    Ports.getHistory
                                        { address = w.address
                                        , mint = tk
                                        }

                                else
                                    Cmd.none
                            )
                    )
                )

        VerifyBackpackCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | backpackInProgress = False
                            , bpkWarning = Just "Profile not found."
                          }
                        , logHttpError "VerifyBackpackCb" err
                        )
                    )
                    (\val ->
                        ( { model
                            | profile = Nothing
                            , backpackInProgress = False
                            , wallet = Just val
                            , view = ViewSettings
                          }
                        , Cmd.none
                        )
                    )

        VerifySolCb res ->
            res
                |> unpack
                    (\_ ->
                        ( { model
                            | warning = Just "Wallet not found."
                            , solDomainInProgress = Nothing
                          }
                        , Cmd.none
                        )
                    )
                    (\val ->
                        ( { model
                            | solDomainInProgress = Nothing
                            , profile =
                                model.solDomainInProgress
                                    |> Maybe.map
                                        (\domain ->
                                            { address = val
                                            , label = Just <| domain
                                            , meta =
                                                { name = ".sol Domain"
                                                , icon = "/tokens/solana.png"
                                                }
                                            }
                                        )
                          }
                        , Cmd.none
                        )
                    )

        HistoryCb res ->
            res
                |> unpack
                    (\_ ->
                        ( { model
                            | fetchingHistory = Nothing
                          }
                        , Cmd.none
                        )
                    )
                    (\val ->
                        ( { model
                            | history =
                                model.history
                                    |> Dict.insert
                                        (val.mintAddr
                                            |> Maybe.withDefault "SOL"
                                        )
                                        val.history
                            , balances =
                                model.balances
                                    |> Dict.insert
                                        (val.mintAddr
                                            |> Maybe.withDefault "SOL"
                                        )
                                        val.balance
                            , fetchingHistory = Nothing
                          }
                        , Cmd.none
                        )
                    )

        SetDomainText v ->
            ( { model
                | profile = Nothing
                , warning = Nothing
                , fields =
                    model.fields
                        |> Dict.insert "name" v
              }
            , Cmd.none
            )

        SetBackpackText v ->
            ( { model
                | fields =
                    model.fields
                        |> Dict.insert "bpk" v
                , bpkWarning = Nothing
              }
            , Cmd.none
            )

        CancelConnect ->
            ( { model | profile = Nothing }
            , Cmd.none
            )

        ConnectSelect ->
            ( { model
                | profile = Nothing
                , wallet = model.profile
                , view = ViewSettings
              }
            , Cmd.none
            )

        CopyQR val ->
            ( model
            , Ports.copyQRToClipboard val
            )

        ShareQR val ->
            ( model
            , Ports.shareQR val
            )

        PaymentCb val ->
            ( { model | success = Just val }
            , Cmd.none
            )

        Disconnect ->
            ( { model
                | wallet = Nothing
                , view = ViewHome
                , history = Dict.empty
                , balances = Dict.empty
                , fields = Dict.empty
              }
            , if Maybe.andThen .label model.wallet /= Nothing then
                Cmd.none

              else
                Ports.disconnect ()
            )

        GetWallets ->
            ( { model | view = ViewWallets }
            , if model.walletOptions == Nothing || model.walletOptions == Just [] then
                Ports.getWallets ()

              else
                Cmd.none
            )

        WalletsCb ws ->
            ( { model | walletOptions = Just ws }
            , Cmd.none
            )

        Connect val ->
            ( { model | connectInProgress = Just val }
            , Ports.connect val
            )

        ConnectCb addr ->
            let
                wallet =
                    Maybe.map2
                        (\meta addr_ ->
                            { meta = meta
                            , address = addr_
                            , label = Nothing
                            }
                        )
                        (model.connectInProgress
                            |> Maybe.andThen
                                (\name ->
                                    model.walletOptions
                                        |> Maybe.withDefault []
                                        |> List.Extra.find (.name >> (==) name)
                                )
                        )
                        addr
            in
            wallet
                |> unwrap
                    ( { model
                        | connectInProgress = Nothing
                      }
                    , Cmd.none
                    )
                    (\walletAddr ->
                        ( { model
                            | wallet = Just walletAddr
                            , connectInProgress = Nothing
                            , view =
                                if model.active == Nothing then
                                    ViewHome

                                else
                                    ViewInput
                          }
                        , Cmd.none
                        )
                    )

        RefreshHistory ->
            ensureWallet
                (\w ->
                    model.currentToken
                        |> unwrap ( model, Cmd.none )
                            (\curr ->
                                ( { model
                                    | fetchingHistory = Just curr
                                  }
                                , Ports.getHistory
                                    { address = w.address
                                    , mint = curr
                                    }
                                )
                            )
                )

        VerifyToken ->
            let
                val =
                    Misc.get "token" model.fields
            in
            if String.isEmpty val then
                ( { model | warning = Just "Field is empty!" }
                , Cmd.none
                )

            else if
                List.any (\x -> x.address == Just val)
                    (model.tokens ++ Misc.tokens)
            then
                ( { model | warning = Just "This token has already been added." }
                , Cmd.none
                )

            else
                ( { model | warning = Nothing, verifyInProgress = True }
                , Ports.verifyToken val
                )

        TokenCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | warning = Just err
                            , verifyInProgress = False
                          }
                        , Cmd.none
                        )
                    )
                    (\tk ->
                        ( { model
                            | tokens = tk :: model.tokens
                            , tokenAdded = Just tk
                            , verifyInProgress = False
                            , fields =
                                model.fields
                                    |> Dict.insert "token" ""
                          }
                        , Cmd.none
                        )
                    )

        ZoneCb c ->
            ( { model | zone = c }, Cmd.none )

        SetHistory tokenAddr ->
            ensureWallet
                (\w ->
                    let
                        isCurrent =
                            Just tokenAddr == model.currentToken

                        shouldFetch =
                            model.fetchingHistory
                                == Nothing
                                && (Dict.get
                                        (tokenAddr
                                            |> Maybe.withDefault "SOL"
                                        )
                                        model.history
                                        == Nothing
                                   )
                    in
                    ( { model
                        | currentToken =
                            if isCurrent then
                                Nothing

                            else
                                Just tokenAddr
                        , fetchingHistory =
                            if shouldFetch then
                                Just tokenAddr

                            else
                                model.fetchingHistory
                      }
                    , if shouldFetch then
                        Ports.getHistory
                            { address = w.address
                            , mint = tokenAddr
                            }

                      else
                        Cmd.none
                    )
                )

        SetLang c ->
            ( { model | language = c }
            , Ports.setValue
                ( "language"
                , case c of
                    Eng ->
                        "eng"

                    Esp ->
                        "esp"
                )
            )

        OpenLink x ->
            ( model, Ports.openLink x )

        SetView v ->
            ( { model
                | view = v
                , warning = Nothing
                , tokenAdded = Nothing
                , fields = Dict.empty
              }
            , Cmd.none
            )

        SetActive v ->
            if model.wallet == Nothing then
                ( { model | view = ViewWallets, active = Just v }
                , if model.walletOptions == Nothing || model.walletOptions == Just [] then
                    Ports.getWallets ()

                  else
                    Cmd.none
                )

            else
                ( { model
                    | active = Just v
                    , view = ViewInput
                    , fields =
                        model.fields
                            |> Dict.insert "nums" ""
                  }
                , Cmd.none
                )

        SubmitAmount ->
            ensureWallet
                (\wallet ->
                    let
                        val =
                            Misc.get "nums" model.fields
                                |> String.toFloat
                    in
                    val
                        |> unwrap
                            ( model, Cmd.none )
                            (\n ->
                                ( { model | qrInProgress = True }
                                , Ports.buildTx
                                    { recipient = wallet.address
                                    , amount = String.fromFloat n
                                    , splToken =
                                        model.active
                                            |> Maybe.andThen .address
                                    , message = "Thank you for using Nugget Pay 💸"

                                    --, label = "my-label"
                                    --, memo = "my-memo"
                                    }
                                )
                            )
                )

        VerifySol ->
            let
                val =
                    model.fields
                        |> Misc.get "name"
                        |> String.replace " " ""

                isErr =
                    val
                        |> String.split "."
                        |> List.length
                        |> (/=) 2
            in
            if String.isEmpty val then
                ( model, Cmd.none )

            else if isErr then
                ( { model
                    | warning = Just "Invalid input."
                  }
                , Cmd.none
                )

            else
                ( { model
                    | profile = Nothing
                    , warning = Nothing
                    , solDomainInProgress = Just val
                  }
                , Ports.solDomain val
                )

        VerifyBackpack ->
            let
                val =
                    model.fields
                        |> Misc.get "bpk"
            in
            if String.isEmpty val then
                ( model, Cmd.none )

            else
                ( { model
                    | profile = Nothing
                    , bpkWarning = Nothing
                    , backpackInProgress = True
                  }
                , Http.task
                    { method = "GET"
                    , headers = []
                    , url = "https://xnft-api-server.xnfts.dev/v1/users/fromUsername?username=" ++ val
                    , body = Http.emptyBody
                    , resolver =
                        JD.at [ "user", "id" ] JD.string
                            |> jsonResolver
                    , timeout = Nothing
                    }
                    |> Task.andThen
                        (\id ->
                            Http.task
                                { method = "GET"
                                , headers = []
                                , url = "https://xnft-api-server.xnfts.dev/v1/users?user_id=" ++ id
                                , body = Http.emptyBody
                                , resolver =
                                    JD.map2 Tuple.pair
                                        (JD.field "publicKey" JD.string)
                                        (JD.field "blockchain" JD.string)
                                        |> JD.list
                                        |> JD.field "publicKeys"
                                        |> JD.map
                                            (List.filterMap
                                                (\( pk, blockchain ) ->
                                                    if blockchain /= "solana" then
                                                        Nothing

                                                    else
                                                        Just pk
                                                )
                                            )
                                        |> JD.andThen
                                            (List.head
                                                >> unwrap
                                                    (JD.fail "Profile not found")
                                                    JD.succeed
                                            )
                                        |> JD.andThen
                                            (\addr ->
                                                JD.field "image" JD.string
                                                    |> JD.map
                                                        (\img ->
                                                            { address = addr
                                                            , label = Just val
                                                            , meta =
                                                                { name = "Backpack Username"
                                                                , icon = img
                                                                }
                                                            }
                                                        )
                                            )
                                        |> JD.field "user"
                                        |> jsonResolver
                                , timeout = Nothing
                                }
                        )
                    |> Task.attempt VerifyBackpackCb
                )

        SetText k v ->
            ( { model
                | fields =
                    model.fields
                        |> Dict.insert k v
              }
            , Cmd.none
            )

        BuildTxCb val ->
            ( { model
                | qrCode = val
                , success = Nothing
                , qrInProgress = False
              }
            , if val == Nothing then
                Ports.clearWatch ()

              else
                Cmd.none
            )


logHttpError : String -> Http.Error -> Cmd msg
logHttpError tag =
    Helpers.Http.parseError >> logWithTag tag


logWithTag : String -> String -> Cmd msg
logWithTag tag err =
    Ports.log (tag ++ ":\n" ++ err)
