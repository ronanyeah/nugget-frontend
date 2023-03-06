module Misc exposing (..)

import Dict
import Types exposing (..)


get k xs =
    Dict.get k xs
        |> Maybe.withDefault ""


tokens =
    [ buildToken "solana.png" "Solana" "SOL" 9 Nothing
    , buildToken "usdc.png" "USDC" "USDC" 6 (Just "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v")
    , buildToken "bonk.png" "Bonk" "BONK" 5 (Just "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263")
    , buildToken "bsol.webp" "BlazeStake" "BSOL" 9 (Just "bSo13r4TkiE4KumL71LsHTPpL2euBYLFx6h9HP3piy1")
    , buildToken "shdw.png" "Shadow" "SHDW" 9 (Just "SHDWyBxihqiCj6YekG2GUr7wqKLeLAMK1gHZck9pL6y")
    , buildToken "orca.png" "Orca" "ORCA" 6 (Just "orcaEKTdK7LKz57vaAYr9QeNsVEPfiu6QeMU1kektZE")
    , buildToken "hades.jpg" "Hades" "HADES" 9 (Just "BWXrrYFhT7bMHmNBFoQFWdsSgA3yXoAnMhDK6Fn1eSEn")
    , buildToken "rlb.webp" "Rollbit" "RLB" 9 (Just "RLBxxFkseAZ4RgJH3Sqn8jXxhmGoz9jWxDNJMh8pL7a")
    , buildToken "dust.png" "Dust" "DUST" 9 (Just "DUSTawucrTsGU8hcqRdHDCbuYhCPADMLM2VcCb8VnFnQ")
    ]


buildToken a b c d e =
    { img = "/tokens/" ++ a
    , name = b
    , symbol = c
    , decimals = d
    , address = e
    }
