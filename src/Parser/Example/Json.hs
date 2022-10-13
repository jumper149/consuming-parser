{-# LANGUAGE QualifiedDo #-}

module Parser.Example.Json where

import Parser.Combinators qualified as P
import Parser.Core qualified as P
import Parser.Core.Consumption qualified as P
import Parser.Core.Error qualified as P
import Parser.TypeError qualified as P ()

import Data.Kind

type Value :: Type
data Value where
  MkValueObject :: Object -> Value
  MkValueArray :: Array -> Value
  MkValueText :: Text -> Value
  MkValueNumber :: () -> Value
  MkValueBoolean :: Bool -> Value
  MkValueNull :: Value
  deriving stock (Show)

type Object :: Type
data Object where
  MkObjectEmpty :: Object
  MkObjectCons :: String -> Value -> Object -> Object
  deriving stock (Show)

type Array :: Type
data Array where
  MkArrayEmpty :: Array
  MkArrayCons :: Value -> Array -> Array
  deriving stock (Show)

type Text :: Type
newtype Text = MkText String
  deriving stock (Show)

-- number
--     integer fraction exponent

-- integer
--     digit
--     onenine digits
--     '-' digit
--     '-' onenine digits

-- digits
--     digit
--     digit digits

-- digit
--     '0'
--     onenine

-- onenine
--     '1' . '9'

-- fraction
--     ""
--     '.' digits

-- exponent
--     ""
--     'E' sign digits
--     'e' sign digits

-- sign
--     ""
--     '+'
--     '-'

type Error :: Type
type Error = String

jsonError :: Error -> P.Parser c Char Error a
jsonError = P.throw . P.ErrorCustom . ("JSON: " ++)

pJson :: P.Parser P.Consuming Char Error Value
pJson = pValue P.<* P.end

pValue :: P.Parser P.Consuming Char Error Value
pValue = P.do
  pWhitespace
  value <-
    (MkValueObject P.<$> pObject)
      P.<|> (MkValueArray P.<$> pArray)
      P.<|> (MkValueText P.<$> pText)
      P.<|> (MkValueNumber P.<$> pNumber)
      P.<|> (MkValueBoolean P.<$> pBoolean)
      P.<|> (MkValueNull P.<$ pNull)
  pWhitespace
  P.pure value

pObject :: P.Parser P.Consuming Char Error Object
pObject = pObject' P.<|> jsonError "Object"

pObject' :: P.Parser P.Consuming Char Error Object
pObject' = P.do
  P.match '{'
  (pWhitespace P.>> pObjectEnd) P.<|> pObjectContent
 where
  pObjectContent :: P.Parser P.Consuming Char Error Object
  pObjectContent = P.do
    pWhitespace
    key <- pKey
    pWhitespace
    P.match ':'
    value <- pValue
    object <- pObjectEnd P.<|> (P.match ',' P.>> pObjectContent)
    P.pure $ MkObjectCons key value object
  pObjectEnd :: P.Parser P.Consuming Char Error Object
  pObjectEnd = P.do
    P.match '}'
    P.pure MkObjectEmpty

pKey :: P.Parser P.Consuming Char Error String
pKey = pString P.<|> jsonError "Key"

pArray :: P.Parser P.Consuming Char Error Array
pArray = pArray' P.<|> jsonError "Array"

pArray' :: P.Parser P.Consuming Char Error Array
pArray' = P.do
  P.match '['
  (pWhitespace P.>> pArrayEnd) P.<|> pArrayContent
 where
  pArrayContent :: P.Parser P.Consuming Char Error Array
  pArrayContent = P.do
    value <- pValue
    array <- pArrayEnd P.<|> (P.match ',' P.>> pArrayContent)
    P.pure $ MkArrayCons value array
  pArrayEnd :: P.Parser P.Consuming Char Error Array
  pArrayEnd = P.do
    P.match ']'
    P.pure MkArrayEmpty

pText :: P.Parser P.Consuming Char Error Text
pText = (MkText P.<$> pString) P.<|> jsonError "Text"

-- TODO: Implement.
pNumber :: P.Parser P.Consuming Char Error ()
pNumber = jsonError "Number"

pBoolean :: P.Parser P.Consuming Char Error Bool
pBoolean = ((pFalse P.>> P.pure False) P.<|> (pTrue P.>> P.pure True)) P.<|> jsonError "Boolean"

pFalse :: P.Parser P.Consuming Char Error ()
pFalse = (P.match 'f' P.>> P.match 'a' P.>> P.match 'l' P.>> P.match 's' P.>> P.match 'e') P.<|> jsonError "False"

pTrue :: P.Parser P.Consuming Char Error ()
pTrue = (P.match 't' P.>> P.match 'r' P.>> P.match 'u' P.>> P.match 'e') P.<|> jsonError "True"

pNull :: P.Parser P.Consuming Char Error ()
pNull = (P.match 'n' P.>> P.match 'u' P.>> P.match 'l' P.>> P.match 'l') P.<|> jsonError "Null"

pString :: P.Parser P.Consuming Char Error String
pString = P.do
  P.match '"'
  (text, _) <- (pRegular P.<|> pEscaped) `P.manyTill` P.match '"'
  P.pure text
 where
  pRegular :: P.Parser P.Consuming Char Error Char
  pRegular =
    P.token P.>>= \case
      '\\' -> jsonError "Backslash"
      '"' -> jsonError "Quotation mark"
      x -> P.pure x
  pEscaped :: P.Parser P.Consuming Char Error Char
  pEscaped =
    (P.match '\\' P.>>) $
      pQuotationMark
        P.<|> pBackslash
        P.<|> pSlash
        P.<|> pBackspace
        P.<|> pFormfeed
        P.<|> pLinefeed
        P.<|> pCarriageReturn
        P.<|> pTab
        P.<|> pUnicode
   where
    pQuotationMark :: P.Parser P.Consuming Char Error Char
    pQuotationMark = P.match '"' P.>> P.pure '"'
    pBackslash :: P.Parser P.Consuming Char Error Char
    pBackslash = P.match '\\' P.>> P.pure '\\'
    pSlash :: P.Parser P.Consuming Char Error Char
    pSlash = P.match '/' P.>> P.pure '/'
    pBackspace :: P.Parser P.Consuming Char Error Char
    pBackspace = P.match 'b' P.>> P.pure '\b'
    pFormfeed :: P.Parser P.Consuming Char Error Char
    pFormfeed = P.match 'f' P.>> P.pure '\f'
    pLinefeed :: P.Parser P.Consuming Char Error Char
    pLinefeed = P.match 'n' P.>> P.pure '\n'
    pCarriageReturn :: P.Parser P.Consuming Char Error Char
    pCarriageReturn = P.match 'r' P.>> P.pure '\r'
    pTab :: P.Parser P.Consuming Char Error Char
    pTab = P.match 't' P.>> P.pure '\t'
    pUnicode :: P.Parser P.Consuming Char Error Char
    pUnicode = P.do
      P.match 'u'
      x1 <- pHexDigit
      x2 <- pHexDigit
      x3 <- pHexDigit
      x4 <- pHexDigit
      let num = (((((x1 * 16) + x2) * 16) + x3) * 16) + x4
      P.pure $ toEnum num
     where
      pHexDigit :: P.Parser P.Consuming Char Error Int
      pHexDigit =
        P.token P.>>= \case
          '0' -> P.pure 0
          '1' -> P.pure 1
          '2' -> P.pure 2
          '3' -> P.pure 3
          '4' -> P.pure 4
          '5' -> P.pure 5
          '6' -> P.pure 6
          '7' -> P.pure 7
          '8' -> P.pure 8
          '9' -> P.pure 9
          'a' -> P.pure 10
          'A' -> P.pure 10
          'b' -> P.pure 11
          'B' -> P.pure 11
          'c' -> P.pure 12
          'C' -> P.pure 12
          'd' -> P.pure 13
          'D' -> P.pure 13
          'e' -> P.pure 14
          'E' -> P.pure 14
          'f' -> P.pure 15
          'F' -> P.pure 15
          _ -> jsonError "Hex digit"

pWhitespace :: P.Parser P.Unknown Char Error ()
pWhitespace = P.void $ P.many $ P.oneOf ['\x0020', '\x000A', '\x000D', '\x0009']

f :: P.Parser P.Unknown Char Error ()
f = P.void $ P.many $ P.oneOf ['\x0020', '\x000A', '\x000D', '\x0009']
