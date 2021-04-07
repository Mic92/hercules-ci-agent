{-# LANGUAGE CPP #-}

module Hercules.Agent.Compat (katipLevel) where

import qualified Katip as K

katipLevel :: K.Severity -> K.PermitFunc

#if MIN_VERSION_katip(0,8,0)
katipLevel =
  K.permitItem
#else
katipLevel x = x
#endif