{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Anki (doAnki) where

import           Control.Monad         (when)
import           Data.Aeson            (FromJSON, ToJSON, Value (..), object,
                                        (.=))
import qualified Data.ByteString.Char8 as S8
import           Data.HashMap.Strict   ((!))
import           Data.Maybe            (mapMaybe)
import           Data.Text             (Text, concat, pack, unpack)
import qualified Data.Vector           as V
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple   (getResponseBody, httpJSON,
                                        setRequestBodyJSON)
import           Prelude               hiding (concat, (!))
import           Text.RawString.QQ
import           Text.Regex            (mkRegex, subRegex)

import           Parse                 (Card (..), isCloze, parseAluffi)

----------------------------------
doAnki :: IO ()
doAnki = do mkReq "createDeck" (object ["deck" .= ("aluffi" :: String)]) False
            cards <- parseAluffi
            mapM_ addNote cards
            removeUnseen $ map cardTitle cards
----------------------------------
fields :: Card -> Value
fields c = object $ ["Title"  .= cardTitle c,
                    "Chapter" .= process (cardChap c),
                    "Section" .= process ( cardSect c),
                    "Part"    .= maybe "" (unpack . process) (cardPart c) ,
                    "Page"    .= maybe "" show (cardPage c) ]
                    ++ (if isCloze c then x else y)
  where x = ["Text" .= process ( concat [cardFront c, "\n<br><hr><br>\n",cardBack c])]
        y = ["Front" .= process ( cardFront c), "Back" .= process ( cardBack c)]


cssdefault = ".card {\n font-family: times new roman;\n font-size: 30px;\n text-align: center;\n color: black;\n background-color: white;\n line-height: 40px;\n}\n"

addNote :: Card -> IO()
addNote c = do (Array noteids) <- mkReq "findNotes" (object ["query" .= concat ["deck:aluffi and title:" , cardTitle c]]) False
               when (length noteids > 1) $ do error $ show ("Duplicates found!",c,noteids)
               when (length noteids == 1) $ do
                   let [noteid] = V.toList noteids
                   let note = object ["deckName"  .= ("aluffi" :: String),
                                      "modelName" .= (if isCloze c then "Cloze" else "Basic" :: String),
                                      "fields"    .= fields c,
                                      "tags"      .= [show $ cardType c],
                                      "id"        .= noteid]
                   mkReq "updateNoteFields" (object ["note" .= note]) False
                   return ()
               when (length noteids == 0) $ do
                   mkReq "addNote" obj False
                   return ()
               return ()
 where obj = object ["note" .= note]
       note = object ["deckName"  .= ("aluffi" :: String),
                     "modelName"  .= (if isCloze c then "Cloze" else "Basic" :: String),
                     "fields"     .= fields c,
                     "tags"       .= [show $ cardType c]]

-- Any Titles that are NOT found in the current list of titles are removed
removeUnseen :: [Text] -> IO ()
removeUnseen ids = do cardIDs <- req1
                      (Array req) <- mkReq "cardsInfo" (object ["cards" .= cardIDs]) False
                      let unseen = mapMaybe (checkSeen ids) $ V.toList req
                      when (length unseen > 0) $ do
                          putStrLn $ "remove " ++ show (length unseen) ++ " notes? y/n -->"
                          myString <- getLine
                          when (head myString == 'y') $ do
                                mkReq "deleteNotes" (object ["notes" .= unseen]) False
                                return ()
                      return ()

  where req1 = mkReq "findCards" (object ["query" .= ("deck:aluffi" :: String)]) False

-- AKA: partial function city, population 8
checkSeen :: [Text] -> Value -> Maybe Value
checkSeen ids (Object x) = case x ! "fields" of
                  Object y -> case y ! "Title" of
                      Object z ->  case z ! "value" of
                          String s -> if s `elem` ids
                                        then Nothing
                                        else Just (x ! "note")


-- Combine all text processing
process :: Text -> Text
process = foldr (.) id (map sub subs)

sub :: (String,String) -> Text ->  Text
sub (pat,val) x = pack $ subRegex (mkRegex pat) (unpack x) val

subs :: [(String,String)]
subs = [("##([^#]+)##",[r|{{c1::\1}}|]), -- Make cloze deletions for anything ## like this ##
        ("\\$([^$]+)\\$",[r|[$$]\1[/$$]|]), -- Anki LaTeX delimiters
        ([r|\/([^\$\/]+)\/|],[r|<i>\1</i>|]),-- Convert /italic text/ into <i>italic text</i>
        ("\n","<br>")]

--------------------------------------------------------------------

-- | Send JSON request to Anki, receive JSON value
mkReq :: String -> Value -> Bool -> IO Value
mkReq action params debug = do
    let req = object [ "action" .= action,"version".=(6 :: Int), "params" .= params]
    let request = setRequestBodyJSON req  "GET http://localhost:8765"
    response <- httpJSON request
    let (Object x) = getResponseBody response
    when debug $ S8.putStrLn $ Yaml.encode x
    return $ x ! "result"
