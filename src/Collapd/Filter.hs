{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Collapd.Filter
  ( attr
  , toFilter
  , ldapFilter
  ) where

import           Prelude hiding (filter)

import           Control.Applicative ((<|>), many)
import           Control.Monad (guard)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Text.Encoding (decodeUtf8)
import           Ldap.Client as Ldap

import qualified Debug.Trace as DT

deriving instance Show Ldap.Filter

word :: AP.Parser B.ByteString
word = B.concat <$> AP.many1' (digit
                           <|> escapedAscii
                           <|> letter_ascii)

-- wordPermissive allows spaces and equal signs
wordPermissive :: AP.Parser B.ByteString
wordPermissive = B.concat <$> AP.many1' (digit
                           <|> escapedAscii
                           <|> special_characters
                           <|> letter_ascii)

_pack :: AP.Parser Char -> AP.Parser B.ByteString
_pack p = do
  c <- p
  return $ B.pack [c]

digit :: AP.Parser B.ByteString 
digit = _pack AP.digit

letter_ascii :: AP.Parser B.ByteString
letter_ascii = _pack AP.letter_ascii

escapedAscii :: AP.Parser B.ByteString
escapedAscii = do
  backslash <- AP.char '\\'
  first <- hexa
  second <- hexa
  return $ B.pack [backslash, first, second]

special_characters :: AP.Parser B.ByteString
special_characters = _pack . F.foldl1 (<|>) . fmap AP.char $ charsToEscape
  where
    charsToEscape :: String
    charsToEscape = " ,.-_="

hexa :: AP.Parser Char
hexa = AP.satisfy isHexa <?> "hexa"

isHexa :: Char -> Bool
isHexa c = AP.isDigit c || (c >= 'a' && c <= 'f')

toFilter :: B.ByteString -> Maybe Filter
toFilter = AP.maybeResult . (flip AP.feed "") . AP.parse ldapFilter

ldapFilter :: AP.Parser Filter
ldapFilter =
  (   filterAnd
  <|> filterOr
  <|> filterNot
  <|> filterItem
  )

attr :: AP.Parser Attr
attr = Attr <$> decodeUtf8 <$> word

parens :: AP.Parser a -> AP.Parser a
parens p = AP.char '(' *> p <* AP.char ')'

filterAnd :: AP.Parser Filter
filterAnd = parens $ AP.char '&' >> And <$> filterList

filterOr :: AP.Parser Filter
filterOr = parens $ AP.char '|' >> Or <$> filterList

filterNot :: AP.Parser Filter
filterNot = parens $ AP.char '!' >> Not <$> ldapFilter

filterList :: AP.Parser (NonEmpty Filter)
filterList = fromList <$> AP.many1' ldapFilter

filterItem :: AP.Parser Filter
filterItem = filterPresent <|> filterSubstring <|> filterSimple

filterSimple :: AP.Parser Filter
filterSimple = filterEqual <|> filterApprox <|> filterGreater <|> filterLess

filterEqual   :: AP.Parser Filter
filterEqual   = _operator "="  (:=)

filterApprox  :: AP.Parser Filter
filterApprox  = _operator "~=" (:~=)

filterGreater :: AP.Parser Filter
filterGreater = _operator ">=" (:>=)

filterLess    :: AP.Parser Filter
filterLess    = _operator "<=" (:<=)

_operator :: forall a.
          B.ByteString -> (Attr -> B.ByteString -> a) -> AP.Parser a
_operator match op = parens $ do
  attr' <- attr
  _ <- AP.string match
  value <- wordPermissive
  return $ attr' `op` value

filterPresent :: AP.Parser Filter
filterPresent = parens $ Present <$> (attr <* AP.string "=*")

filterSubstring :: AP.Parser Filter
filterSubstring = parens $ do
    attr' <- attr
    _ <- AP.char '='
    initial <- AP.option Nothing $ Just <$> wordPermissive
    _ <- AP.char '*'
    any' <- AP.many' $ wordPermissive <* AP.char '*'
    guard $ check initial any'
    final <- AP.option Nothing $ Just <$> wordPermissive
    return $ attr' :=* (initial, any', final)
  where
    check :: Maybe B.ByteString -> [B.ByteString] -> Bool
    check Nothing [] = False
    check _ _ = True

-- TODO implement if needed
--      extensible = attr [":dn"] [":" matchingrule] ":=" value
--                   / [":dn"] ":" matchingrule ":=" value

-- From RFC2254
-- 4. String Search Filter Definition
--
-- The string representation of an LDAP search filter is defined by the
-- following grammar, following the ABNF notation defined in [5].  The
-- filter format uses a prefix notation.
--
--      filter     = "(" filtercomp ")"
--      filtercomp = and / or / not / item
--      and        = "&" filterlist
--      or         = "|" filterlist
--      not        = "!" filter
--      filterlist = 1*filter
--      item       = simple / present / substring / extensible
--      simple     = attr filtertype value
--      filtertype = equal / approx / greater / less
--      equal      = "="
--      approx     = "~="
--      greater    = ">="
--      less       = "<="
--      extensible = attr [":dn"] [":" matchingrule] ":=" value
--                   / [":dn"] ":" matchingrule ":=" value
--      present    = attr "=*"
--      substring  = attr "=" [initial] any [final]
--      initial    = value
--      any        = "*" *(value "*")
--      final      = value
--      attr       = AttributeDescription from Section 4.1.5 of [1]
--      matchingrule = MatchingRuleId from Section 4.1.9 of [1]
--      value      = AttributeValue from Section 4.1.6 of [1]
--
-- The attr, matchingrule, and value constructs are as described in the
-- corresponding section of [1] given above.
