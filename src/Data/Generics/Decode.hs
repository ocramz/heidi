{-# LANGUAGE
    DeriveFunctor,
    GeneralizedNewtypeDeriving
#-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Decode
-- Description :  Composable decoding of terms
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Composable decoding of terms
-----------------------------------------------------------------------------
module Data.Generics.Decode (Decode, mkDecode, runDecode, (>>>)) where

import Control.Applicative      (Alternative(..))
import Control.Category (Category(..)) 
-- import Data.Foldable (Foldable(..), asum)
import Control.Monad ((>=>))
-- import           Data.Maybe               (fromMaybe)
-- import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..), Severity, logDebug, logInfo, logWarning, logError, runLoggingT, PureLoggingT(..), runPureLoggingT)
-- import Control.Monad.Trans.State.Strict (StateT(..), runStateT)

import Prelude hiding ((.))


-- | We can decouple lookup and value conversion and have distinct error behaviour.
-- Multiple value decoding functions can be combined via the Alternative instance.
--
-- Note : 'Decode' is called Kleisli in base.Control.Arrow; among other things it has a Profunctor instance.

newtype Decode m i o = Decode { runDecode_ :: i -> m o } deriving (Functor)

-- | Run a decoding function
runDecode :: Decode m i o -> i -> m o
runDecode = runDecode_

-- | Construct a 'Decode' from a monadic arrow.
mkDecode :: (i -> m o) -> Decode m i o
mkDecode = Decode

instance Applicative m => Applicative (Decode m i) where
  pure x = Decode $ \ _ -> pure x
  Decode af <*> Decode aa = Decode $ \ v -> af v <*> aa v

instance Alternative m => Alternative (Decode m i) where
  empty = Decode $ const empty
  Decode p <|> Decode q = Decode $ \v -> p v <|> q v

-- | This instance is copied from @Kleisli@ (defined in Control.Arrow)
instance Monad m => Category (Decode m) where
  id = Decode return
  (Decode f) . (Decode g) = Decode (g >=> f)

-- | Left-to-right composition
--
-- @(>>>) :: Monad m => Decode m a b -> Decode m b c -> Decode m a c@
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
(>>>) = flip (.)
{-# inline (>>>) #-}



-- | [NOTE Key lookup + value conversion, behaviour of row functions ]
--
-- If the key is not found /or/ the conversion fails, use a default; the first exception thrown by the lookup-and-convert function will be rethrown.
-- We'd like instead to try many different decoders, and only throw if /all/ have failed
-- 
-- How should Alternative behave for lookup-and-convert that both might fail?
-- 
-- value decoding : try all decoders, return first successful (== Alternative)
-- decoding missing values : should be configurable
-- -- * use default value
-- -- * skip rows that have any missing value
-- 
-- row function: decoding behaviour should be defined for all function arguments
--
-- example : 
--
-- λ> bool (2 :: Int) (VBool False) <|> bool (2 :: Int) (VBool True)
-- False
-- λ> bool (2 :: Int) (VDouble 32) <|> bool (2 :: Int) (VBool True)
-- *** Exception: ValueTypeError 2 VTypeBool (VDouble 32.0)
--
-- λ> Nothing <|> pure 32.0
-- Just 32.0
-- λ> (bool (2 :: Int) (VDouble 32) <|> bool (2 :: Int) (VBool True)) :: Maybe Bool
-- Just True
--
-- ^ throwM in IO : strict (the first failing decoder throws an exception), Maybe : lazy (keeps trying decoders and returns the first successful one)
