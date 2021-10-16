{-#language OverloadedStrings#-}
{-#language RecordWildCards#-}

module Main where

import Text.LaTeX
import Text.LaTeX.Packages.Hyperref
import Text.LaTeX.Packages.TabularX
import Data.Fixed
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Environment
import Text.Pretty.Simple
import Text.Read

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then print "No arguments provided!"
        else do input <- readFile (args !! 0)
                infoString <- readFile (args !! 1)
                let timesheetFilePath = args !! 2
                let periods = read input :: [Period]
                -- let Info {..} = read info :: Info
                let info = read infoString :: Info
                writeFile timesheetFilePath $ Text.unpack $ (render :: LaTeX -> Text) $ execLaTeXM $ do
                    documentclass [] article
                    usepackage ["colorlinks=true"] hyperref
                    usepackage [] tabularxp
                    usepackage ["left=2cm, right=2cm, top=2cm"] "geometry"
                    document $ do
                        noindent
                        makeInvoiceBlock info periods
                        lnbkspc (Ex 3)
                        makeSenderBlock info
                        lnbkspc (Ex 3)
                        makeRecipientBlock info
                        -- infoTable info
                        lnbkspc (Ex 3)
                        -- entriesToTable info periods
                        -- (sequence_ . fmap periodToTable) periods
                        (sequence_ . periodsToTable) periods

defaultTimeFormat = "%h:%0M"

instance Texy NominalDiffTime where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultTimeFormat

defaultDayFormat = "%d of %B %Y"

instance Texy Day where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultDayFormat

data Info = Info
    { senderName :: Text
    , senderAddress :: Text
    , senderCity :: Text
    , recipientName :: Text
    , recipientAddress :: Text
    , recipientCity :: Text
    , payRate :: Double
    , currency :: Text
    , invoiceDate :: Day
    , invoiceNumber :: Text
    } deriving (Show, Read)

data Ticket = Issue String Int | PullRequest String Int deriving (Show, Read)

instance Texy Ticket where
    texy (Issue repository n) =
        href [] (createURL ("https://github.com/" <> repository <> "/issues/" <> show n)) ("#" <> texy n)
    texy (PullRequest repository n) =
        href [] (createURL ("https://github.com/" <> repository <> "/pull/" <> show n)) ("#" <> texy n)

data Period = Period
    { start, end :: Day
    , tasks :: [Task]
    , services :: [Service]
    } deriving (Show, Read)

data Task = Task
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Service = Service
    { weight :: Double
    , name :: Text
    , price :: Double
    } deriving (Show, Read)

makeHeader :: Day -> Day -> LaTeXM ()
makeHeader start end = do
    (textbf . center) $ do
        texy start
        "---"
        texy end
        lnbkspc (Ex 2)

-- makeInvoiceBlock :: Info -> [Period] -> LaTeXM ()
-- makeInvoiceBlock info periods = do
--     (textbf . small) "INVOICE" >> lnbk >> texy (invoiceNumber info) >> lnbkspc (Ex 2)
--     (textbf . small) "DATE" >> lnbk >> texy (invoiceDate info) >> lnbkspc (Ex 2)
--     (textbf . small) "BALANCE DUE" >> lnbk >> texy (currency info) >> " "
--     (texy . sum . fmap (makeTotalAmount info)) periods >> lnbkspc (Ex 3)

makeInvoiceBlock :: Info -> [Period] -> LaTeXM ()
makeInvoiceBlock info periods = do
    (textbf . small) "INVOICE" >> lnbk >> texy (invoiceNumber info) >> lnbkspc (Ex 2)
    (textbf . small) "DATE" >> lnbk >> texy (invoiceDate info) >> lnbkspc (Ex 2)
    (textbf . small) "BALANCE DUE" >> lnbk >> texy (currency info) >> " "
    (texy . sum . fmap makeTotalAmount) periods >> lnbkspc (Ex 3)

makeSenderBlock :: Info -> LaTeXM ()
makeSenderBlock info = do
    textbf . large3 $ texy (senderName info) >> lnbkspc (Ex 1)
    texy (senderAddress info) >> lnbk
    texy (senderCity info) >> lnbkspc (Ex 3)

makeRecipientBlock :: Info -> LaTeXM ()
makeRecipientBlock info = do
    (textbf . small) "BILL TO" >> lnbkspc (Ex 2)
    (textbf . large2) (texy (recipientName info)) >> lnbkspc (Ex 1)
    texy (recipientAddress info) >> lnbk
    texy (recipientCity info) >> lnbkspc (Ex 3)

-- makeTotalAmount :: Info -> Period -> Double
-- makeTotalAmount info Period {..} = (sum . fmap (\Service {..} -> price * weight)) services
--     where
--         allHours :: Double
--         allHours = (sum . fmap (realToFrac . (/3600) . nominalDiffTimeToSeconds . time)) tasks

makeTotalAmount :: Period -> Double
makeTotalAmount Period {..} = (sum . fmap (\Service {..} -> price * weight)) services
    where
        allHours :: Double
        allHours = (sum . fmap (realToFrac . (/3600) . nominalDiffTimeToSeconds . time)) tasks

infoTable :: Info -> LaTeXM ()
infoTable info = tabular Nothing [CenterColumn, CenterColumn] $ do
    makeSenderBlock info & makeRecipientBlock info >> lnbkspc (Ex 2)
    -- hline >> lnbk
    -- "1" & "2" >> lnbkspc (Ex 2)
    -- (textbf . small) "SERVICE" & (textbf . small) "RATE" & (textbf . small) "QUANTITY" & "" & (textbf . small) "AMOUNT" >> lnbkspc (Ex 2)
    -- lnbk
    -- hline

-- entriesToTable :: Info -> [Period] -> LaTeXM ()
-- entriesToTable info periods = tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", CenterColumn, NameColumn "X", RightColumn] $ do
--     hline >> lnbk
--     (textbf . small) "SERVICE" & (textbf . small) "RATE" & (textbf . small) "QUANTITY" & (textbf . small) "AMOUNT" >> lnbkspc (Ex 2)
--     hline >> lnbk
--     -- sequence_ $ fmap (entryToRow info) (superMerge . groupSimilarEntriesSorted $ xs)
--     sequence_ $ fmap periodToRows periods
--     lnbk
--     hline >> lnbk
--     (textbf . small) "TOTAL"
--         & ""
--         & ""
--         & do
--               texy (currency info)
--               " "
--               (texy . sum . fmap (makeTotalAmount info)) periods >> lnbkspc (Ex 2)
--     hline

-- periodToRows :: Period -> LaTeXM ()
-- periodToRows Period {..} = (sequence_ . fmap serviceToRow) services

serviceToRow :: Service -> LaTeXM ()
serviceToRow Service {..} = texy name
    & texy price
    & texy weight
    & do "USD" >> " " >> (texy (price * weight)) >> lnbkspc (Ex 3)

periodsToTable :: [Period] -> [LaTeXM ()]
periodsToTable periods = fmap (\period -> periodToTable (start period) (end period) period) periods

periodToTable :: Day -> Day -> Period -> LaTeXM ()
periodToTable start end period = do
    makeHeader start end
    tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", CenterColumn, NameColumn "X", RightColumn] $ do
        hline >> lnbk
        (textbf . small) "SERVICE" & (textbf . small) "RATE" & (textbf . small) "QUANTITY" & (textbf . small) "AMOUNT" >> lnbkspc (Ex 2)
        hline >> lnbk
        (sequence_ . fmap serviceToRow) (services period)
        lnbk
        hline >> lnbk
        (textbf . small) "TOTAL"
            & ""
            & ""
            & do
                "USD "
                (texy . makeTotalAmount) period >> lnbkspc (Ex 2)
        hline
        lnbkspc (Ex 31)



-- this one is actually not only for fractional
prettyStringForFractional x
    | length afterDotWithoutZeros == 0 = beforeDot ++ afterDotWithoutZeros
    | head afterDotWithoutZeros == '0' = beforeDot ++ afterDotWithoutZeros
    | otherwise = beforeDot ++ "." ++ afterDotWithoutZeros
        -- where beforeDot = take dotIndex (show x)
        where beforeDot = case dotIndex of Nothing -> show x
                                           Just _ -> take (fromJust dotIndex) (show x)
              afterDot = case dotIndex of Nothing -> ""
                                          Just _ -> drop (fromJust dotIndex + 1) (show x)
              dotIndex = List.elemIndex '.' (show x)
              afterDotWithoutZeros = dropWhile (== '0') (reverse afterDot)
