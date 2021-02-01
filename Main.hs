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

-- main :: IO ()
-- main = do
--     example <- getContents
--     let Log {..} = read example :: Log
--     Text.putStrLn $ (render :: LaTeX -> Text) $ execLaTeXM $ do
--         documentclass [] article
--         usepackage ["colorlinks=true"] hyperref
--         usepackage [] tabularxp
--         document $ do
--             makeHeader start end
--             entriesToTable entries

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then print "No arguments provided!"
        else do log <- readFile (args !! 0)
                infoString <- readFile (args !! 1)
                let timesheetFilePath = args !! 2
                let Log {..} = read log :: Log
                -- let Info {..} = read info :: Info
                let info = read infoString :: Info
                writeFile timesheetFilePath $ Text.unpack $ (render :: LaTeX -> Text) $ execLaTeXM $ do
                    documentclass [] article
                    usepackage ["colorlinks=true"] hyperref
                    usepackage [] tabularxp
                    document $ do
                        makeHeader2 start end
                        entriesToTable entries info


defaultTimeFormat = "%h:%0M"

-- instance Read NominalDiffTime where
--     readPrec = do
--         String s <- lexP
--         parseTimeM False defaultTimeLocale defaultTimeFormat s

instance Texy NominalDiffTime where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultTimeFormat

defaultDayFormat = "%d of %B %Y"

instance Texy Day where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultDayFormat

data Log = Log
    { start, end :: Day
    , entries :: [Entry]
    } deriving (Show, Read)

data Entry = Entry
    { serviceName :: Text
    , description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

mergeTwoEntriesByServiceName :: Entry -> Entry -> Entry
mergeTwoEntriesByServiceName entry1 entry2 = entry2 {time = time entry1 + time entry2}

mergeEntriesByServiceName :: [Entry] -> Entry
mergeEntriesByServiceName entries = (head entries) {time = sum (fmap time entries)}

data Info = Info
    { senderName :: Text
    , senderAddress :: Text
    , senderCity :: Text
    , recipientName :: Text
    , recipientAddress :: Text
    , recipientCity :: Text
    , payRate :: Double
    , date :: Day
    } deriving (Show, Read)

data Ticket = Issue String Int | PullRequest String Int deriving (Show, Read)

instance Texy Ticket where
    texy (Issue repository n) =
        href [] (createURL ("https://github.com/" <> repository <> "/issues/" <> show n)) ("#" <> texy n)
    texy (PullRequest repository n) =
        href [] (createURL ("https://github.com/" <> repository <> "/pull/" <> show n)) ("#" <> texy n)

makeHeader :: Day -> Day -> LaTeXM ()
makeHeader start end = do
    textbf $ do
        "Time Sheet for "
        texy start
        "---"
        texy end
        lnbkspc (Ex 2)

makeHeader2 :: Day -> Day -> LaTeXM ()
makeHeader2 start end = do
    textbf $ do
        "Invoice for "
        texy start
        "---"
        texy end
        lnbkspc (Ex 2)

entriesToTable :: [Entry] -> Info -> LaTeXM ()
entriesToTable xs info = tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", CenterColumn, NameColumn "X", NameColumn "X", RightColumn] $ do
    hline
    -- "Service" & "Rate" & "Quantity" & "" & "Amount" >> lnbk
    "Service" & "Rate" & "Quantity" & "" & "Amount" >> lnbk
    hline
    -- version with `mergeEntriesByServiceName`
    texy (serviceName (mergeEntriesByServiceName xs))
        & fromString (prettyStringForFractional (payRate info))
        & fromString (prettyStringForFractional ((/3600) . nominalDiffTimeToSeconds $ time (mergeEntriesByServiceName xs)))
        & ""
        & fromString (prettyStringForFractional ((/3600) . nominalDiffTimeToSeconds $ time (mergeEntriesByServiceName xs))) >> lnbk
    -- version where all entries are just printed without merging ones with the same `serviceName`
    -- sequence_ $ fmap (entryToRow info) xs
    hline
    "Total" & "" & "" & "" & texy (sum (fmap time xs)) >> lnbk
    hline

-- this one is empty so it works
-- entriesToTable :: [Entry] -> Info -> LaTeXM ()
-- entriesToTable xs info = tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", CenterColumn, NameColumn "X", NameColumn "X", RightColumn] $ do
--     hline
--     -- "Service" & "Rate" & "Quantity" & "" & "Amount" >> lnbk
--     "Service" & "Rate" & "Quantity" & "" & "Amount" >> lnbk
--     hline
--     texy (serviceName (mergeEntriesByServiceName xs))
--         & ""
--         & ""
--         & ""
--         & fromString (prettyStringForFractional (time (mergeEntriesByServiceName xs))) >> lnbk
--     -- sequence_ $ fmap (entryToRow info) xs
--     -- "Software development services"
--     --     & "$10.00"
--     --     -- & texy ((read :: String -> Double) . showFixed True . (/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs))
--     --     & fromString (prettyStringForFractional ((/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs)))
--     --     & ""
--     --     -- & texy ((sum (fmap time xs)) * 10) >> lnbk
--     --     -- & texy (((read :: String -> Double) . showFixed True . (/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs)) * 10) >> lnbk
--     --     & fromString (prettyStringForFractional (10 * ((/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs)))) >> lnbk
--     hline
--     "Total" & "" & "" & "" & texy (sum (fmap time xs)) >> lnbk
--     hline

entryToRow :: Info -> Entry -> LaTeXM ()
entryToRow info Entry {..} = texy serviceName
    & fromString (prettyStringForFractional (payRate info))
    & fromString (prettyStringForFractional quantity)
    & ""
    & fromString (prettyStringForFractional amount) >> lnbk
        where quantity = (/3600) . nominalDiffTimeToSeconds $ time
              amount = realToFrac (payRate info) * quantity

-- prettyStringForFractional x
--     | length afterDotWithoutZeros == 0 = beforeDot ++ afterDotWithoutZeros
--     | head afterDotWithoutZeros == '0' = beforeDot ++ afterDotWithoutZeros
--     | otherwise = beforeDot ++ "." ++ afterDotWithoutZeros
--         where beforeDot = take dotIndex (show x)
--               afterDot = drop (dotIndex + 1) (show x)
--               dotIndex = fromJust (List.elemIndex '.' (show x))
--               afterDotWithoutZeros = dropWhile (== '0') (reverse afterDot)

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
