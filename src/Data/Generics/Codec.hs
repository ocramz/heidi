module Data.Generics.Codec (
  realM, scientificM, textM, stringM, oneHotM, int, int8, int16, int32, int64, word, word8, word16, word32, word64, 
  -- * TC (Type and Constructor annotation)
  TC(..), tcTyN, tcTyCon, mkTyN, mkTyCon
  -- * Exceptions
  , TypeError(..)) where

import Control.Applicative (Alternative(..))

import Control.Monad.Catch (MonadThrow(..))

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

-- scientific
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
-- text
import qualified Data.Text as T


import Data.Generics.Encode.Internal (VP(..), getIntM, getInt8M, getInt16M, getInt32M, getInt64M, getWordM, getWord8M, getWord16M, getWord32M, getWord64M, getBoolM, getFloatM, getDoubleM, getScientificM, getCharM, getStringM, getTextM, getOneHotM, TypeError(..), TC(..), tcTyN, tcTyCon, mkTyN, mkTyCon)
import Data.Generics.Decode (Decode, mkDecode)
import Data.Generics.Encode.OneHot (OneHot)






-- decInt :: D.Decode Maybe VP Int
-- decInt = D.mkDecode getInt
-- -- decInteger = D.mkDecode getInteger
-- decDouble :: D.Decode Maybe VP Double
-- decDouble = D.mkDecode getDouble
-- -- decChar = D.mkDecode getChar
-- -- decText = D.mkDecode getText

-- | Decode any real numerical value (integer, double, float or 'Scientific') into a Double
realM :: (Alternative m, MonadThrow m) => Decode m VP Double
realM =
  (fromIntegral <$> int)     <|>
  double                     <|>
  (realToFrac <$> float)     <|>
  (toRealFloat <$> scientific) 

-- | Decode any real numerical value (integer, double, float or 'Scientific') into a Scientific
scientificM :: (Alternative m, MonadThrow m) => Decode m VP Scientific
scientificM =
  scientific <|>
  (fromFloatDigits . fromIntegral <$> int) <|>
  (fromFloatDigits <$> double)             <|>
  (fromFloatDigits <$> float)

-- | Decode a string ('String' or 'Text') into a Text
textM :: (Alternative m, MonadThrow m) => Decode m VP T.Text
textM =
  (T.pack <$> string) <|>
  text

-- | Decode a string ('String' or 'Text') into a String
stringM :: (Alternative m, MonadThrow m) => Decode m VP String
stringM =
  string <|>
  (T.unpack <$> text)





-- | Decode a one-hot value
oneHotM :: MonadThrow m => Decode m VP (OneHot Int)
oneHotM = mkDecode getOneHotM





int :: MonadThrow m => Decode m VP Int
int = mkDecode getIntM

int8 :: MonadThrow m => Decode m VP Int8
int8 = mkDecode getInt8M

int16 :: MonadThrow m => Decode m VP Int16
int16 = mkDecode getInt16M

int32 :: MonadThrow m => Decode m VP Int32
int32 = mkDecode getInt32M

int64 :: MonadThrow m => Decode m VP Int64
int64 = mkDecode getInt64M

word :: MonadThrow m => Decode m VP Word
word = mkDecode getWordM

word8 :: MonadThrow m => Decode m VP Word8
word8 = mkDecode getWord8M

word16 :: MonadThrow m => Decode m VP Word16
word16 = mkDecode getWord16M

word32 :: MonadThrow m => Decode m VP Word32
word32 = mkDecode getWord32M

word64 :: MonadThrow m => Decode m VP Word64
word64 = mkDecode getWord64M

bool :: MonadThrow m => Decode m VP Bool
bool = mkDecode getBoolM

float :: MonadThrow m => Decode m VP Float
float = mkDecode getFloatM

double :: MonadThrow m => Decode m VP Double
double = mkDecode getDoubleM

scientific :: MonadThrow m => Decode m VP Scientific
scientific = mkDecode getScientificM

char :: MonadThrow m => Decode m VP Char
char = mkDecode getCharM

string :: MonadThrow m => Decode m VP String
string = mkDecode getStringM

text :: MonadThrow m => Decode m VP T.Text
text = mkDecode getTextM



