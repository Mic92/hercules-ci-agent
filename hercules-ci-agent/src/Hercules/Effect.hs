{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hercules.Effect where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Hercules.API.Id (Id, idText)
import Hercules.Agent.Sensitive (Sensitive (Sensitive, reveal), revealContainer)
import Hercules.CNix (Derivation)
import Hercules.CNix.Store (getDerivationArguments, getDerivationBuilder, getDerivationEnv)
import Hercules.Effect.Container (BindMount (BindMount))
import qualified Hercules.Effect.Container as Container
import Hercules.Error (escalateAs)
import qualified Hercules.Formats.Secret as Formats.Secret
import Katip (KatipContext, Severity (..), logLocM, logStr)
import Protolude
import System.FilePath
import UnliftIO.Directory (createDirectory, createDirectoryIfMissing)

parseDrvSecretsMap :: Map ByteString ByteString -> Either Text (Map Text Text)
parseDrvSecretsMap drvEnv =
  case drvEnv & M.lookup "secretsMap" of
    Nothing -> pure mempty
    Just secretsMapText -> case A.eitherDecode (BL.fromStrict secretsMapText) of
      Left _ -> Left "Could not parse secretsMap variable in derivation. It must be a JSON dictionary of strings referencing agent secret names."
      Right r -> Right r

-- | Write secrets to file based on secretsMap value
writeSecrets :: (MonadIO m, KatipContext m) => Maybe FilePath -> Map Text Text -> Map Text (Sensitive Formats.Secret.Secret) -> FilePath -> m ()
writeSecrets sourceFileMaybe secretsMap extraSecrets destinationDirectory = write . fmap reveal . addExtra =<< gather
  where
    addExtra = flip M.union extraSecrets
    write = liftIO . BS.writeFile (destinationDirectory </> "secrets.json") . BL.toStrict . A.encode
    gather =
      if null secretsMap
        then pure mempty
        else do
          allSecrets <-
            sourceFileMaybe & maybe (purer mempty) \sourceFile -> do
              secretsBytes <- liftIO $ BS.readFile sourceFile
              case A.eitherDecode $ BL.fromStrict secretsBytes of
                Left e -> do
                  logLocM ErrorS $ "Could not parse secrets file " <> logStr sourceFile <> ": " <> logStr e
                  throwIO $ FatalError "Could not parse secrets file as configured on agent."
                Right r -> pure (Sensitive r)

          createDirectoryIfMissing True destinationDirectory
          secretsMap & M.traverseWithKey \destinationName (secretName :: Text) -> do
            case revealContainer (allSecrets <&> M.lookup secretName) of
              Nothing ->
                liftIO $
                  throwIO $
                    FatalError $
                      "Secret " <> secretName <> " does not exist, so we can't find a secret for " <> destinationName <> ". Please make sure that the secret name matches a secret on your agents."
              Just ssecret ->
                pure do
                  secret <- ssecret
                  -- Currently this is `id` but we might want to fork the
                  -- format here or omit some fields.
                  pure $
                    Formats.Secret.Secret
                      { data_ = Formats.Secret.data_ secret
                      }

data RunEffectParams = RunEffectParams
  { runEffectDerivation :: Derivation,
    runEffectToken :: Maybe (Sensitive Text),
    runEffectSecretsConfigPath :: Maybe FilePath,
    runEffectApiBaseURL :: Text,
    runEffectDir :: FilePath,
    runEffectProjectId :: Maybe (Id "project"),
    runEffectProjectPath :: Maybe Text
  }

(=:) :: k -> a -> Map k a
(=:) = M.singleton

runEffect :: (MonadThrow m, KatipContext m) => RunEffectParams -> m ExitCode
runEffect p@RunEffectParams {runEffectDerivation = derivation, runEffectSecretsConfigPath = secretsPath, runEffectApiBaseURL = apiBaseURL, runEffectDir = dir} = do
  drvBuilder <- liftIO $ getDerivationBuilder derivation
  drvArgs <- liftIO $ getDerivationArguments derivation
  drvEnv <- liftIO $ getDerivationEnv derivation
  drvSecretsMap <- escalateAs FatalError $ parseDrvSecretsMap drvEnv
  let mkDir d = let newDir = dir </> d in toS newDir <$ createDirectory newDir
  buildDir <- mkDir "build"
  etcDir <- mkDir "etc"
  secretsDir <- mkDir "secrets"
  runcDir <- mkDir "runc-state"
  let extraSecrets =
        runEffectToken p
          & maybe
            mempty
            ( \token ->
                "hercules-ci" =: do
                  tok <- token
                  pure $
                    Formats.Secret.Secret
                      { data_ = M.singleton "token" $ A.String tok
                      }
            )
  writeSecrets secretsPath drvSecretsMap extraSecrets (toS secretsDir)
  liftIO $ do
    -- Nix sandbox sets tmp to buildTopDir
    -- Nix sandbox reference: https://github.com/NixOS/nix/blob/24e07c428f21f28df2a41a7a9851d5867f34753a/src/libstore/build.cc#L2545
    --
    -- TODO: what if we have structuredAttrs?
    -- TODO: implement passAsFile?
    let overridableEnv, onlyImpureOverridableEnv, fixedEnv :: Map Text Text
        overridableEnv =
          M.fromList $
            [ ("PATH", "/path-not-set"),
              ("HOME", "/homeless-shelter"),
              ("NIX_STORE", "/nix/store"), -- TODO store.storeDir
              ("NIX_BUILD_CORES", "1"), -- not great
              ("NIX_REMOTE", "daemon"),
              ("IN_HERCULES_CI_EFFECT", "true"),
              ("HERCULES_CI_API_BASE_URL", apiBaseURL),
              ("HERCULES_CI_SECRETS_JSON", "/secrets/secrets.json")
            ]
              <> [("HERCULES_CI_PROJECT_ID", idText x) | x <- toList $ runEffectProjectId p]
              <> [("HERCULES_CI_PROJECT_PATH", x) | x <- toList $ runEffectProjectPath p]

        -- NB: this is lossy. Consider using ByteString-based process functions
        drvEnv' = drvEnv & M.mapKeys (decodeUtf8With lenientDecode) & fmap (decodeUtf8With lenientDecode)
        impureEnvVars = mempty -- TODO
        fixedEnv =
          M.fromList
            [ ("NIX_LOG_FD", "2"),
              ("TERM", "xterm-256color")
            ]
        onlyImpureOverridableEnv =
          M.fromList
            [ ("NIX_BUILD_TOP", "/build"),
              ("TMPDIR", "/build"),
              ("TEMPDIR", "/build"),
              ("TMP", "/build"),
              ("TEMP", "/build")
            ]
        (//) :: Ord k => Map k a -> Map k a -> Map k a
        (//) = flip M.union
    Container.run
      runcDir
      Container.Config
        { extraBindMounts =
            [ BindMount {pathInContainer = "/build", pathInHost = buildDir, readOnly = False},
              BindMount {pathInContainer = "/etc", pathInHost = etcDir, readOnly = False},
              BindMount {pathInContainer = "/secrets", pathInHost = secretsDir, readOnly = True},
              BindMount {pathInContainer = "/etc/resolv.conf", pathInHost = "/etc/resolv.conf", readOnly = False},
              BindMount {pathInContainer = "/nix/var/nix/daemon-socket/socket", pathInHost = "/nix/var/nix/daemon-socket/socket", readOnly = True}
            ],
          executable = decodeUtf8With lenientDecode drvBuilder,
          arguments = map (decodeUtf8With lenientDecode) drvArgs,
          environment = overridableEnv // drvEnv' // onlyImpureOverridableEnv // impureEnvVars // fixedEnv,
          workingDirectory = "/build",
          hostname = "hercules-ci",
          rootReadOnly = False
        }
