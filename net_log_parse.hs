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

module Main where

import Text.ParserCombinators.Parsec

data DottedQuad = DottedQuad {
    first  :: Int
  , second :: Int
  , third  :: Int
  , fourth :: Int
} deriving (Show)

data DateTime = DateTime {
    year    :: Int
  , month   :: String
  , day     :: Int
  , hour    :: Int
  , min     :: Int
  , sec     :: Int
  , offset  :: String
} deriving (Show)

type HttpResp = Int

data ConnectAttempt = ConnectAttempt {
    remoteAddr :: DottedQuad
  , accessTime :: DateTime
  , command    :: String
  , response   :: HttpResp
  , port       :: Int
} deriving (Show)

-- HTTP connection log parser.
httpAccessLog = endBy connectAttempt eol
connectAttempt = do remoteAddr <- symbol dottedQuad
                    unknown1   <- symbol (char '-')
                    unknown2   <- symbol (char '-')
                    accessTime <- symbol dateTime
                    command    <- symbol quotedVal
                    response   <- symbol httpResponse
                    many (noneOf "\n\r")
                    return $ ConnectAttempt remoteAddr accessTime command response 0
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"
symbol p = skipMany space >> p
quotedVal = between (char '"') (char '"') (many (noneOf "\""))
dottedQuad = do
    first  <- octet
    char '.'
    second <- octet
    char '.'
    third  <- octet
    char '.'
    fourth <- octet
    return $ DottedQuad first second third fourth
octet = do
    res <- int
    if res < 256 then return res
    else fail "Octet value out of range."
monthDay = do
    res <- int
    if res < 32 then return res
    else fail "Day of month out of range."
int = do
    digits <- many1 digit
    return (read digits)
httpResponse = int
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

parseHTTPAccessLog :: String -> Either ParseError [ConnectAttempt]
parseHTTPAccessLog = parse httpAccessLog "(unknown)"

main :: IO()
main = do
    logEntries <- getContents
    case parse httpAccessLog "(stdin)" logEntries of
        Left e  -> do putStrLn "Error parsing input:"
                      print e
        Right r -> mapM_ print r

