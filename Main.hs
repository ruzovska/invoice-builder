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
    example <- getContents
    let Log {..} = read example :: Log
    Text.putStrLn $ (render :: LaTeX -> Text) $ execLaTeXM $ do
        documentclass [] article
        usepackage ["colorlinks=true"] hyperref
        usepackage [] tabularxp
        document $ do
            makeHeader2 start end
            entriesToTable entries

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
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
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

entriesToTable :: [Entry] -> LaTeXM ()
entriesToTable xs = tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", CenterColumn, NameColumn "X", NameColumn "X", RightColumn] $ do
    hline
    "Description" & "Rate" & "Quantity" & "" & "Amount" >> lnbk
    hline
    -- sequence_ $ fmap entryToRow xs
    "Software development services"
        & "$10.00"
        -- & texy ((read :: String -> Double) . showFixed True . (/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs))
        & fromString (prettyPrintForFractional ((/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs)))
        & ""
        -- & texy ((sum (fmap time xs)) * 10) >> lnbk
        -- & texy (((read :: String -> Double) . showFixed True . (/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs)) * 10) >> lnbk
        & fromString (prettyPrintForFractional (10 * ((/3600) . nominalDiffTimeToSeconds $ sum (fmap time xs)))) >> lnbk
    hline
    "Total" & "" & "" & "" & texy (sum (fmap time xs)) >> lnbk
    hline

entryToRow :: Entry -> LaTeXM ()
entryToRow Entry {..} = texy description & (if isDone then "completed" else "") & sequence_ (List.intersperse newline (fmap (mbox . texy) tickets)) & "" & texy time >> lnbk


-- prettyPrintForFractional :: Fractional a => a -> String
-- prettyPrintForFractional x = beforeDot ++ dropWhile (== '0') (reverse afterDot)
--     where beforeDot = take (dotIndex + 1) (show x)
--           afterDot = drop (dotIndex + 1) (show x)
--           dotIndex = fromJust (List.elemIndex '.' (show x))

-- prettyPrintForFractional x = if (head afterDotWithoutZeros == '0' || length afterDotWithoutZeros == 0)
--     then (beforeDot ++ afterDotWithoutZeros)
--     else (beforeDot ++ "." ++ afterDotWithoutZeros)
--         where beforeDot = take dotIndex (show x)
--               afterDot = drop (dotIndex + 1) (show x)
--               dotIndex = fromJust (List.elemIndex '.' (show x))
--               afterDotWithoutZeros = dropWhile (== '0') (reverse afterDot)

-- prettyPrintForFractional x = case afterDotWithoutZeros of
--     ((length afterDotWithoutZeros) == 0) -> beforeDot ++ "." ++ afterDotWithoutZeros
--     head afterDotWithoutZeros == '0' -> beforeDot ++ afterDotWithoutZeros
--     xs -> beforeDot ++ "." ++ afterDotWithoutZeros
--         where beforeDot = take dotIndex (show x)
--               afterDot = drop (dotIndex + 1) (show x)
--               dotIndex = fromJust (List.elemIndex '.' (show x))
--               afterDotWithoutZeros = dropWhile (== '0') (reverse afterDot)

prettyPrintForFractional x
    | length afterDotWithoutZeros == 0 = beforeDot ++ afterDotWithoutZeros
    | head afterDotWithoutZeros == '0' = beforeDot ++ afterDotWithoutZeros
    | otherwise = beforeDot ++ "." ++ afterDotWithoutZeros
        where beforeDot = take dotIndex (show x)
              afterDot = drop (dotIndex + 1) (show x)
              dotIndex = fromJust (List.elemIndex '.' (show x))
              afterDotWithoutZeros = dropWhile (== '0') (reverse afterDot)