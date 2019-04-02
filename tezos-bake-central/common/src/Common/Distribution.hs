{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Common.Distribution where

data Distribution
  = Distribution_FromSource
  | Distribution_Docker
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

distributionMethod :: Distribution
distributionMethod =
#if defined(DOCKER)
  Distribution_Docker
#else
  Distribution_FromSource
#endif
