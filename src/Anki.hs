{-# LANGUAGE OverloadedStrings #-}

module Anki (doAnki) where
import           Control.Monad         (when)
import           Data.Aeson            (FromJSON, ToJSON, Value, object, (.=))
import qualified Data.ByteString.Char8 as S8
import           Data.Text             (Text)
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple   (getResponseBody, httpJSON,
                                        setRequestBodyJSON)

import           Parse                 (Card (..), isCloze, parseAluffi)

----------------------------------
doAnki :: IO ()
doAnki = do cards <- parseAluffi
            mapM_ addNote cards
----------------------------------
fields :: Card -> Value
fields c = error $ show ("undefined field", c)


cssdefault = ".card {\n font-family: times new roman;\n font-size: 30px;\n text-align: center;\n color: black;\n background-color: white;\n line-height: 40px;\n}\n"

addNote :: Card -> IO()
addNote c = do mkReq "addNote" obj True
               return ()
 where obj = object ["deckName"  .= ("aluffi" :: String),
                     "modelName" .= (if isCloze c then "Cloze" else "Basic" :: String),
                     "fields"    .= fields c,
                     "tags"      .= [show $ cardType c]]
--------------------------------------------------------------------
-- Example requests
req'  = mkReq "deckNames" (object []) True
req'' = mkReq "findCards" (object ["query" .= ("deck:aluffi" :: String)]) True

-- | Send JSON request to Anki, receive JSON value
mkReq :: String -> Value -> Bool -> IO Value
mkReq action params debug = do
    let req = object [ "action" .= action,"version".=(6 :: Int), "params" .= params]
    let request = setRequestBodyJSON req  "GET http://localhost:8765"
    response <- httpJSON request
    let x = getResponseBody response :: Value
    when debug $ S8.putStrLn $ Yaml.encode x
    return x
