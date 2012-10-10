-----------------------------------------------------------------------------
--
-- Module      :  Json
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Json (

) where

data JsonValue = JsonString String |
    JsonNumber Double |
    JsonObject [(String,JsonValue)] |
    JsonArray [JsonValue] |
    JsonNull |
    JsonBool Bool

instance Show JsonValue where
    showsPrec _ (JsonString str) = showsPrec 1 str
    showsPrec _ (JsonNumber d) = showsPrec 1 d
    showsPrec _ (JsonObject o) = \s -> ("{" ++ foldr (\(n,v) str -> show n ++ ":" ++ (showsPrec 1 o str)) "}" o ++ s)
    showsPrec _ (JsonArray a) =  showsPrec 1 a
    showsPrec _ JsonNull = \s -> "null" ++ s
    showsPrec _ (JsonBool b) = \s -> (if b then "true" else "false") ++ s

getString :: JsonValue -> Maybe String
getString (JsonString s) = Just s
getString _ = Nothing

getNumber :: JsonValue -> Maybe Double
getNumber (JsonNumber n) = Just n
getNumber _ = Nothing

----------------------
-- parsing
----------------------
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

type ParseState = String

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> String -> Either String a
parse p s = case (runParse p) s of
    Left err -> Left err
    Right (a, _) -> Right a

parseString :: Parse String
parseString = Parse (\s -> Right (parseString' s))

parseString' :: String -> (String, String)
parseString' s@('"' : _) = ("", s)
parseString' ('\\' : c : s) = let (s1, s2) = parseString' s in (c : s1, s2)
parseString' (c : s) = let (s1, s2) = parseString' s in (c : s1, s2)
parseString' "" = ("", "")

parseJsonValue :: Parse JsonValue
parseJsonValue = undefined

parseQuote :: Parse ()
parseQuote = Parse (\s -> case s of
    '"' : s' -> Right ((), s')
    _ -> Left "Expected \"")

instance Monad Parse where
    a >>= p = Parse (\s -> case (runParse a) s of
        Left err -> Left err
        Right (v, s') -> (runParse (p v) s'))
    return = identity

instance Functor Parse where
    fmap f p = p >>= (\s -> identity (f s))

parseQuotedString :: Parse String
parseQuotedString = do
    parseQuote
    s <- parseString
    parseQuote
    return s

parse' :: String -> Maybe (JsonValue, String)
parse' ('t':'r':'u':'e':s) = Just (JsonBool True, s)
parse' ('f':'a':'l':'s':'e':s) = Just (JsonBool False, s)
parse' ('n':'u':'l':'l':s) = Just (JsonNull, s)
