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

import Text.ParserCombinators.Parsec
import Control.Monad (forM)
import qualified Data.Map.Strict as Map

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
    show dt = "[" ++ show (year dt) ++ "/" ++ show (month dt) ++ "/" ++ show (day dt) ++ " " ++
              show (hour dt) ++ ":" ++ show (minute dt) ++ ":" ++ show (sec dt) ++ " " ++ show (offset dt) ++ "]"

type HttpResp = Int

data ConnectAttempt = ConnectAttempt {
    remoteAddr :: DottedQuad
  , accessTime :: DateTime
  , command    :: String
  , response   :: HttpResp
  , port       :: Int
}
instance Show ConnectAttempt where
    show ca = "From: " ++ show (remoteAddr ca) ++ "  At: " ++ show (accessTime ca) ++
              "  Response: " ++ show (response ca) ++ "  Assigned port: " ++ show (port ca) ++
              "\n\tCommand: " ++ show (command ca)

type HitsByHost = Map.Map DottedQuad Int

parseHTTPAccessLog :: String -> [Either ParseError ConnectAttempt]
parseHTTPAccessLog input = case parse (many line) "(unknown)" input of
                               Left e -> error "Whoops!"
                               Right res -> map (parse connectAttempt "(unknown)") res

hitsByHost :: [Either ParseError ConnectAttempt] -> HitsByHost
hitsByHost = foldl (flip selectiveInsert) Map.empty where
    selectiveInsert x = case x of
                          Left _ -> id
                          Right ca -> Map.insertWith (+) (remoteAddr ca) 1
    
--------------------------------------------------------------------------------
--  HTTP access log parser
--
--  Note: I'm not using applicative parsing, because I need to validate
--  integers against a maximum value, in two places (octet & monthDay).

connectAttempt :: Parser ConnectAttempt
connectAttempt = do remoteAddr <- symbol dottedQuad
                    unknown1   <- symbol (char '-')
                    unknown2   <- symbol (char '-')
                    accessTime <- symbol dateTime
                    command    <- symbol quotedVal
                    response   <- symbol httpResponse
                    many (noneOf "\n\r")
                    return $ ConnectAttempt remoteAddr accessTime command response 0

line :: Parser String
line = do
    res <- many1 (noneOf "\n\r")
    skipMany eol
    return res

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
    first  <- octet
    char '.'
    second <- octet
    char '.'
    third  <- octet
    char '.'
    fourth <- octet
    return $ DottedQuad first second third fourth
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

httpResponse = int

dateTime :: Parser DateTime
dateTime =  do
    char '['
    day <- monthDay
    char '/'
    month <- many1 letter
    char '/'
    year <- int
    char ':'
    hour <- int
    char ':'
    min <- int
    char ':'
    sec <- int
    offset <- many1 (noneOf "]")
    char ']'
    return $ DateTime year month day hour min sec offset
  <?> "date/time"

-- Provide some executable behavior, for quick validation.
main :: IO [()]
main = do
    logEntries <- getContents
    forM (parseHTTPAccessLog logEntries) (\x -> do
        case x of
            Left e -> do
                putStr "Couldn't parse line: "
                print e
            Right ca -> print ca
        putStrLn ""
        )

