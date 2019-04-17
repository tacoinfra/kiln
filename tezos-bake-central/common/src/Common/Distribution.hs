{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Common.Distribution where

data Distribution
  = Distribution_FromSource
  | Distribution_Docker
  | Distribution_LinuxPackage
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

distributionMethod :: Distribution
distributionMethod =
#if defined(DISTRO_DOCKER)
  Distribution_Docker
#elif defined(DISTRO_LINUX_PACKAGE)
  Distribution_LinuxPackage
#else
  Distribution_FromSource
#endif
