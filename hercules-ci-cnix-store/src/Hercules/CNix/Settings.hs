{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.CNix.Settings where

import qualified Data.Set as S
import Hercules.CNix.Encapsulation (moveToForeignPtrWrapper)
import qualified Hercules.CNix.Std.Set as Std.Set
import qualified Hercules.CNix.Std.String as Std.String
import Hercules.CNix.Std.String.Instances ()
import Hercules.CNix.Store.Context (context)
import qualified Language.C.Inline.Cpp as C
import Protolude hiding (evalState, throwIO)

C.context (context <> Std.Set.stdSetCtx <> Std.String.stdStringCtx)

C.include "<set>"
C.include "<string>"

C.include "<nix/config.h>"
C.include "<nix/globals.hh>"

getExtraPlatforms :: IO (Set ByteString)
getExtraPlatforms = do
  s <-
    moveToForeignPtrWrapper
      =<< [C.block| std::set<std::string>*{
      return new nix::StringSet(nix::settings.extraPlatforms.get());
    }|]
  fmap S.fromList $ traverse Std.String.copyToByteString =<< Std.Set.toListFP (s :: Std.Set.StdSet Std.String.CStdString)

-- getSystemFeatures
-- getSubstituters
-- trustedPublicKeys
-- narinfoCacheNegativeTtl
-- netrcFile
