{-# OPTIONS_GHC -Wno-type-defaults #-}
module Core.Data.Row.Decode (decodeRealM, decodeScientificM, decodeTextM, decodeStringM, decOneHotM) where

import Control.Applicative (Alternative(..))
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import qualified Data.Text as T (pack, unpack)
import Data.Text (Text)
import Control.Monad.Catch(MonadThrow(..))

import qualified Data.Generics.Decode as D (Decode, mkDecode)
-- import Data.Generics.Decode (($>))
import Data.Generics.Encode.Internal (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
import Data.Generics.Encode.OneHot (OneHot)


-- decInt :: D.Decode Maybe VP Int
-- decInt = D.mkDecode getInt
-- -- decInteger = D.mkDecode getInteger
-- decDouble :: D.Decode Maybe VP Double
-- decDouble = D.mkDecode getDouble
-- -- decChar = D.mkDecode getChar
-- -- decText = D.mkDecode getText

-- | Decode any real numerical value (integer, double, float or 'Scientific') into a Double
decodeRealM :: (Alternative m, MonadThrow m) => D.Decode m VP Double
decodeRealM =
  (fromIntegral <$> decIntM)     <|>
  decDoubleM                     <|>
  (realToFrac <$> decFloatM)     <|>
  (toRealFloat <$> decScientificM) 

-- | Decode any real numerical value (integer, double, float or 'Scientific') into a Scientific
decodeScientificM :: (Alternative m, MonadThrow m) => D.Decode m VP Scientific
decodeScientificM =
  decScientificM <|>
  (fromFloatDigits . fromIntegral <$> decIntM) <|>
  (fromFloatDigits <$> decDoubleM)             <|>
  (fromFloatDigits <$> decFloatM)             

-- | Decode a string ('String' or 'Text') into a Text
decodeTextM :: (Alternative m, MonadThrow m) => D.Decode m VP Text
decodeTextM =
  (T.pack <$> decStringM) <|>
  decTextM

-- | Decode a string ('String' or 'Text') into a String
decodeStringM :: (Alternative m, MonadThrow m) => D.Decode m VP String
decodeStringM =
  decStringM <|>
  (T.unpack <$> decTextM)


-- | Decode into 'MonadThrow'
decIntM :: MonadThrow m => D.Decode m VP Int
decIntM = D.mkDecode getIntM
decDoubleM :: MonadThrow m => D.Decode m VP Double
decDoubleM = D.mkDecode getDoubleM
decScientificM :: MonadThrow m => D.Decode m VP Scientific
decScientificM = D.mkDecode getScientificM
decFloatM :: MonadThrow m => D.Decode m VP Float
decFloatM = D.mkDecode getFloatM
decStringM :: MonadThrow m => D.Decode m VP String
decStringM = D.mkDecode getStringM
decTextM :: MonadThrow m => D.Decode m VP Text
decTextM = D.mkDecode getTextM

-- | Decode a one-hot value
decOneHotM :: MonadThrow m => D.Decode m VP (OneHot Int)
decOneHotM = D.mkDecode getOneHotM

