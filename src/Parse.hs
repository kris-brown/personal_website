{-# LANGUAGE OverloadedStrings #-}

{-

This will have to be generalized once notes for other textbooks/books become incorporated

-}
module Parse (parseAluffi, Card (..), isCloze) where

import           Data.Attoparsec.Text (Parser, parseOnly, parseTest, string,
                                       takeTill)
import           Data.Maybe           (listToMaybe, mapMaybe)
import           Data.OrgMode.Parse   (parseDocument)
import           Data.OrgMode.Types   (Headline, documentHeadlines, section,
                                       sectionParagraph, subHeadlines, tags,
                                       title)
import           Data.Text            (Text, append, intercalate, isInfixOf,
                                       pack, unpack)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Text.IO         (readFile)
import           Debug.Trace          (trace)
import           Prelude              hiding (readFile)
import           Text.Read            (readMaybe)
-- DATA TYPES
data CardType = Ex | Prop | Def | Question  deriving (Eq,Show)

data Card = Card {
    cardType  :: CardType,
    cardTitle :: Text,
    cardFront :: Text,
    cardBack  :: Text,
    cardChap  :: Text,
    cardSect  :: Text,
    cardPart  :: Maybe Text,
    cardPage  :: Maybe Int
} deriving (Eq,Show)

isCloze :: Card -> Bool
isCloze c = isInfixOf "##" $ append (cardFront c) (cardBack c)

-- | Parse a top-level (i.e. chapter) headline of Aluffi notes
parseAluffi :: IO [Card]
parseAluffi = concatMap parseChap . init <$>  getDoc

-- | Read in org mode file and parse
getDoc :: IO [Headline]
getDoc = do
    doctxt <- readFile "/Users/ksb/aluffi.org"
    case  parseOnly (parseDocument []) doctxt of
        Left err  -> error err
        Right doc -> return $ documentHeadlines doc

parseChap :: Headline -> [Card]
parseChap chap = concatMap (parseSection chapName) (subHeadlines chap)
 where chapName = title chap

-- Get all types of cards from each section
parseSection :: Text -> Headline -> [Card]
parseSection chapName sect = concatMap (parsePart chapName sectName) (init sub)
                          ++ mapMaybe (parseQ chapName sectName) (subHeadlines $ last sub)
 where sub = subHeadlines sect
       sectName = title sect

-- Get either Def, Prop, or Ex from a Part
parsePart :: Text ->Text ->Headline -> [Card]
parsePart chapName sectName part = concatMap (parseSubPart chapName sectName partName) (subHeadlines part)
  where partName = title part

parseSubPart :: Text -> Text -> Text ->Headline -> [Card]
parseSubPart chapName sectName partName subPart  = case title subPart of
    "Notes"        -> []
    "Propositions" -> mapMaybe (mkCard Prop chapName sectName partName) (subHeadlines subPart)
    "Definitions"  -> mapMaybe (mkCard Def chapName sectName partName) (subHeadlines subPart)
    "Examples"     -> mapMaybe (mkCard Ex chapName sectName partName) (subHeadlines subPart)
    _              -> error (show(chapName,sectName,partName,subPart))

-- Construct card
mkCard :: CardType ->  Text -> Text -> Text -> Headline -> Maybe Card
mkCard ct chap sect prt hl
 | nocard hl = Nothing
 | otherwise = Just $ Card ct (parseTitle hl) front back chap sect (Just prt) page
 where (front,back) = frontBack ct hl
       page = pageParser hl

parseQ :: Text ->Text -> Headline -> Maybe Card
parseQ chapName sectName ex
  | nocard ex = Nothing
  | otherwise = Just (Card Question (parseTitle ex) front back chapName sectName Nothing page)
  where (front,back) = frontBack Question ex
        page = pageParser ex

nocard :: Headline -> Bool
nocard hl = "nocard" `elem` tags hl || "todo" `elem` tags hl

frontBack :: CardType -> Headline -> (Text,Text)
frontBack ct hl
 | ct == Ex = (getText hl,"") -- No "backside" to an Example
 | ct `elem` [Prop,Def,Question] = case map getText (subHeadlines hl) of
                                        [a,b] -> (a,b) -- These should all have exactly 2 subheadings
                                        _     -> error (show hl)

getText :: Headline -> Text
getText = sectionParagraph . section

parseTitle :: Headline -> Text
parseTitle x = case parseOnly titleParser $ title x of
    Left err  -> error $ show (err,x,title x )
    Right val -> val

-- Extracts text wrapped in <<these things>>
titleParser :: Parser Text
titleParser = do
    string "<<"
    t <- takeTill ( == '>')
    string ">>"
    return t

pageParser :: Headline -> Maybe Int
pageParser = listToMaybe . mapMaybe (readMaybe .unpack ) . tags
