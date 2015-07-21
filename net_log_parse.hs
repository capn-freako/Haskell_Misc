--------------------------------------------------------------------------------
-- | 
-- Module          : Main
-- Note            : Tinkering with networking log parsing.
-- Original author : David Banas
-- Original date   : July 12, 2015
-- 
-- Copyright (c) 2015 David Banas; all rights reserved World wide.
-- 
--------------------------------------------------------------------------------

module Main (
    DottedQuad
  , DateTime
  , HttpResp
  , ConnectAttempt
  , HitsByHost
  , parseHTTPAccessLog
  , hitsByHost
  , main
) where

--import Control.Monad (forM_)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

kNullAddr :: DottedQuad
kNullAddr = DottedQuad 0 0 0 0

kNullTime :: DateTime
kNullTime = DateTime 0 "" 0 0 0 0 ""

--------------------------------------------------------------------------------
--  Public Interface

data DottedQuad = DottedQuad {
    first  :: Int
  , second :: Int
  , third  :: Int
  , fourth :: Int
} deriving (Eq, Ord)
instance Show DottedQuad where
    show dq = show (first dq) ++ "." ++ show (second dq) ++ "." ++ show (third dq) ++ "." ++ show (fourth dq)

data DateTime = DateTime {
    year    :: Int
  , month   :: String
  , day     :: Int
  , hour    :: Int
  , minute  :: Int
  , sec     :: Int
  , offset  :: String
}
instance Show DateTime where
    show dt = printf (month dt) ++ " " ++ show (day dt) ++ ", " ++ show (year dt) ++ " At: " ++
              printf "%02d" (hour dt) ++ ":" ++ printf "%02d" (minute dt) ++ ":" ++ printf "%02d" (sec dt) ++ " " ++ printf (offset dt)

type HttpResp = Int

data ConnectAttempt = ConnectAttempt {
    remoteAddr :: DottedQuad
  , accessTime :: DateTime
  , command    :: String
  , response   :: HttpResp
  , port       :: Int
}
instance Show ConnectAttempt where
    show ca = "From: " ++ show (remoteAddr ca) ++ "  On: " ++ show (accessTime ca) ++
              "  Response: " ++ show (response ca) ++ "  Assigned port: " ++ show (port ca) ++
              "\n\tCommand: " ++ show (command ca) ++ "\n"

type HitsByHost = Map.Map DottedQuad Int

parseHTTPAccessLog :: String -> [ConnectAttempt]
parseHTTPAccessLog input = case parse httpAccessLog "(unknown)" input of
                               Left e -> error (show e)
                               Right res -> res

hitsByHost :: [ConnectAttempt] -> HitsByHost
hitsByHost = foldl (flip selectiveInsert) Map.empty where
    selectiveInsert x = if remoteAddr x == kNullAddr then id
                        else Map.insertWith (+) (remoteAddr x) 1
    
--------------------------------------------------------------------------------
--  HTTP access log parser
--
--  Note: I'm not using applicative parsing, because I need to validate
--  integers against a maximum value, in two places (octet & monthDay).

httpAccessLog :: Parser [ConnectAttempt]
httpAccessLog = connectAttempt `endBy` eol

connectAttempt :: Parser ConnectAttempt
connectAttempt = try (do addr <- symbol dottedQuad
                         _ <- symbol (char '-')
                         _ <- symbol (char '-')
                         time <- symbol dateTime
                         request <- symbol quotedVal
                         resp <- symbol httpResponse
                         portNum <- option 0 (symbol int)
                         _ <- many (noneOf "\n\r")
                         return $ ConnectAttempt addr time request resp portNum
                     ) <|> do _ <- many (noneOf "\n\r")
                              return $ ConnectAttempt kNullAddr kNullTime "" 0 0

eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

symbol :: Parser a -> Parser a
symbol p = skipMany space >> p

quotedVal :: Parser String
quotedVal = between (char '"') (char '"') (many (noneOf "\""))

dottedQuad :: Parser DottedQuad
dottedQuad = do
    o1  <- octet
    _ <- char '.'
    o2 <- octet
    _ <- char '.'
    o3  <- octet
    _ <- char '.'
    o4 <- octet
    return $ DottedQuad o1 o2 o3 o4
  <?> "Dotted quad of octets (i.e. - IP address)"

octet :: Parser Int
octet = do
    res <- int
    if res < 256 then return res
    else fail "Octet value out of range."

monthDay :: Parser Int
monthDay = do
    res <- int
    if res < 32 then return res
    else fail "Day of month out of range."

int :: Parser Int
int = do
    digits <- many1 digit
    return (read digits)

httpResponse :: Parser Int
httpResponse = int

dateTime :: Parser DateTime
dateTime =  do
    _ <- char '['
    theDay <- monthDay
    _ <- char '/'
    theMonth <- many1 letter
    _ <- char '/'
    theYear <- int
    _ <- char ':'
    theHour <- int
    _ <- char ':'
    theMinute <- int
    _ <- char ':'
    theSecond <- int
    theOffset <- many1 (noneOf "]")
    _ <- char ']'
    return $ DateTime theYear theMonth theDay theHour theMinute theSecond theOffset
  <?> "date/time"

-- Provide some executable behavior, for quick validation.
toPrint :: [ConnectAttempt] -> [String]
toPrint res = if length res < 10
    then map show res
    else map printBarGraph sorted where
        sorted@((_, maxHits):_) = List.sortBy (flip (comparing snd)) $ Map.toList $ hitsByHost res
        printBarGraph (host, hits) = show host ++ ('\t' : replicate n '*') where
            n = hits * 56 `div` maxHits

main :: IO ()
main = interact (unlines . toPrint . parseHTTPAccessLog)
        
