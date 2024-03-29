module Types exposing (..)

import Dict exposing (Dict)
import Http
import Time exposing (Zone)


type alias Model =
    { view : View
    , wallet : Maybe Wallet
    , connectInProgress : Maybe String
    , qrInProgress : Bool
    , mobile : Bool
    , short : Bool
    , fields : Dict String String
    , qrCode : Maybe String
    , active : Maybe Token
    , success : Maybe String
    , walletOptions : Maybe (List WalletMeta)
    , tokens : List Token
    , language : Lang
    , warning : Maybe String
    , bpkWarning : Maybe String
    , history : Dict String (List Entry)
    , balances : Dict String Float
    , verifyInProgress : Bool
    , backpackInProgress : Bool
    , solDomainInProgress : Maybe String
    , currentToken : Maybe (Maybe String)
    , fetchingHistory : Maybe (Maybe String)
    , zone : Zone
    , tokenAdded : Maybe Token
    , isXnft : Bool
    , profile : Maybe Wallet
    , shareEnabled : Bool
    }


type alias Flags =
    { screen : Screen
    , tokens : List Token
    , language : Maybe String
    , xnft : Maybe Wallet
    , shareEnabled : Bool
    }


type alias Screen =
    { width : Int
    , height : Int
    }


type alias Wallet =
    { address : String
    , label : Maybe String
    , meta : WalletMeta
    }


type alias WalletMeta =
    { name : String
    , icon : String
    }


type alias Token =
    { img : String
    , name : String
    , symbol : String
    , decimals : Int
    , address : Maybe String
    }


type alias Entry =
    { amount : String
    , mint : String
    , receiver : String
    , sender : String
    , signature : String
    , timestamp : Int
    }


type alias TxParams =
    { recipient : String
    , amount : String
    , splToken : Maybe String
    , message : String

    --, label : String
    --, memo : String
    }


type Msg
    = SubmitAmount
    | SetView View
    | SetText String String
    | BuildTxCb (Maybe String)
    | SetActive Token
    | Connect String
    | ConnectCb (Maybe String)
    | PaymentCb String
    | WalletsCb (List WalletMeta)
    | GetWallets
    | Disconnect
    | SetLang Lang
    | GotoHistory (Maybe (Maybe String))
    | HistoryCb (Result () { mintAddr : Maybe String, history : List Entry, balance : Float })
    | RefreshHistory
    | SetHistory (Maybe String)
    | ZoneCb Zone
    | VerifyToken
    | TokenCb (Result String Token)
    | OpenLink String
    | VerifySol
    | VerifyBackpack
    | VerifySolCb (Result () String)
    | VerifyBackpackCb (Result Http.Error Wallet)
    | CancelConnect
    | ConnectSelect
    | SetDomainText String
    | SetBackpackText String
    | CopyQR String
    | ShareQR String


type View
    = ViewHome
    | ViewWallets
    | ViewInput
    | ViewSettings
    | ViewHistory
    | ViewNewToken


type Lang
    = Eng
    | Esp
