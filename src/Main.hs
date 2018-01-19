{-# LANGUAGE OverloadedStrings #-}
module Main where
-- a simple currency converter (eur,usd,gbp; extendable)

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map

main :: IO ()
main = scotty 3000 $ do
    
    get "/" $ do
        html $ mconcat ["<head>"
                       ,"<title>Currency converter</title>"
                       ,"</head>"
                       ,"<body style=background-color:#f6f4ee;>"
                       ,"<h1>Currency converter</h1>"
                       ,"<form method=POST action=\"convert\">"
                       ,"<input type=text name=amount>"                       
                       ,"<p></p>"
                       ,"<label>From: </label>" 
                       ,"<input type=radio name=fromc value=eur checked>eur</input>"
                       ,"<input type=radio name=fromc value=usd>usd</input>"
                       ,"<input type=radio name=fromc value=gbp>gbp</input>"
                       ,"<br></br>"
                       ,"<label>To: </label>" 
                       ,"<input style=margin-left: 10%; type=radio name=toc value=eur checked>eur</input>"
                       ,"<input type=radio name=toc value=usd>usd</input>"
                       ,"<input type=radio name=toc value=gbp>gbp</input>"
                       ,"<br></br>"
                       ,"<input type=submit>"
                       ,"</form>"
                       ,"</body>"
                       ]

    post "/convert" $ do
        am <- param "amount" -- refers to the input name
        fr <- param "fromc"
        to <- param "toc"
        text $ mconcat [T.pack (show am)
                       ,T.pack fr 
                       ,T.pack (" = ")
                       ,T.pack (maybe "" show ((convertTo to =<< (convertFrom am fr))))
                       ,T.pack to 
                       ]

-- exchange ratios are bound to eur
ratios = Map.fromList[("eur", 1.00000), ("usd", 1.200000), ("gbp",0.890000)]
        
convertFrom :: Double -> String -> Maybe Double
convertFrom n from = case (Map.lookup from ratios) of
            Nothing -> Nothing
            Just x -> Just (n/x)

convertTo ::  String -> Double -> Maybe Double
convertTo to n = case Map.lookup to ratios of 
            Nothing -> Nothing
            Just x -> Just (fromIntegral (floor (100 * (x*n))) / 100)