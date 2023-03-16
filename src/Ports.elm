port module Ports exposing (..)

import Types exposing (..)



-- OUT


port log : String -> Cmd msg


port getWallets : () -> Cmd msg


port setValue : ( String, String ) -> Cmd msg


port connect : String -> Cmd msg


port openLink : String -> Cmd msg


port clearWatch : () -> Cmd msg


port disconnect : () -> Cmd msg


port getHistory : { address : String, mint : Maybe String } -> Cmd msg


port verifyToken : String -> Cmd msg


port buildTx : TxParams -> Cmd msg



-- IN


port buildTxCb : (Maybe String -> msg) -> Sub msg


port connectCb : (Maybe String -> msg) -> Sub msg


port paymentCb : (String -> msg) -> Sub msg


port tokenCb : (Token -> msg) -> Sub msg


port tokenErr : (String -> msg) -> Sub msg


port walletsCb : (List WalletMeta -> msg) -> Sub msg


port historyCb :
    ({ mintAddr : Maybe String
     , history : List Entry
     , balance : Float
     }
     -> msg
    )
    -> Sub msg


port historyErr : (() -> msg) -> Sub msg
