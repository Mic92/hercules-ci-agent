{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module Hercules.Agent.Worker.Effect.Container where

import Control.Lens
import Data.Aeson (Value (String), eitherDecode, encode, object, toJSON)
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.UUID.V4 as UUID
import GHC.IO.Exception (IOErrorType (HardwareFault))
import Protolude
import System.Directory hiding (executable)
import System.FilePath
import System.IO.Error (ioeGetErrorType)
import System.IO.Temp
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process (CreateProcess (..), StdStream (UseHandle), proc, waitForProcess, withCreateProcess)
import System.Process.ByteString

data BindMount = BindMount
  { pathInContainer :: Text,
    pathInHost :: Text,
    readOnly :: Bool
  }

defaultBindMount :: Text -> BindMount
defaultBindMount path = BindMount {pathInContainer = path, pathInHost = path, readOnly = True}

data Config = Config
  { extraBindMounts :: [BindMount],
    executable :: Text,
    arguments :: [Text],
    environment :: Map Text Text,
    workingDirectory :: Text,
    hostname :: Text,
    rootReadOnly :: Bool
  }

effectToRuncSpec :: Config -> Value -> Value
effectToRuncSpec config spec =
  let defaultMounts = [defaultBindMount "/nix/store"]
      mounts =
        foldMap
          ( \bindMount ->
              pure $
                object
                  [ ("destination", String $ pathInContainer bindMount),
                    ("source", String $ pathInHost bindMount),
                    ("type", "bind"),
                    ( "options",
                      toJSON $
                        ["bind" :: Text]
                          <> ["ro" | readOnly bindMount]
                    )
                  ]
          )
          (defaultMounts <> extraBindMounts config)
   in spec
        & key "process" . key "args" .~ toJSON ([executable config] <> arguments config)
        & key "mounts" . _Array %~ (<> mounts)
        & key "process" . key "terminal" .~ toJSON True
        & key "process" . key "env" .~ toJSON (config & environment & M.toList <&> \(k, v) -> k <> "=" <> v)
        & key "process" . key "cwd" .~ toJSON (config & workingDirectory)
        & key "hostname" .~ toJSON (config & hostname)
        & key "root" . key "readonly" .~ toJSON (config & rootReadOnly)

run :: Config -> IO ExitCode
run config = do
  withTempDirectory "." "runc-state" $ \dir -> do
    let runcExe = "runc"
        createConfigJsonSpec =
          (System.Process.proc runcExe ["spec", "--rootless"])
            { cwd = Just dir
            }
        configJsonPath = dir </> "config.json"
    (exit, _out, err) <- readCreateProcessWithExitCode createConfigJsonSpec ""
    case exit of
      ExitSuccess -> pass
      ExitFailure e -> do
        putErrText (toSL err)
        panic $ "Could not create container configuration template. runc terminated with exit code " <> show e
    templateBytes <- BS.readFile configJsonPath
    template <- case eitherDecode (toS templateBytes) of
      Right a -> pure a
      Left e -> throwIO (FatalError $ "decoding runc config.json template: " <> show e)
    let configJson = effectToRuncSpec config template
    BS.writeFile (dir </> "config.json") (toS $ encode configJson)
    createDirectory (dir </> "rootfs")
    name <- do
      uuid <- UUID.nextRandom
      pure $ "hercules-ci-" <> show uuid
    (exitCode, _) <- bracket
      openPseudoTerminal
      ( \(fd1, fd2) -> handle (\e -> const pass (e :: SomeException)) do
          closeFd fd1
          when (fd2 /= fd1) (closeFd fd2)
      )
      $ \(master, terminal) -> do
        concurrently
          ( do
              terminalHandle <- fdToHandle terminal
              let createProcSpec =
                    (System.Process.proc runcExe ["run", name])
                      { std_in = UseHandle terminalHandle, -- can't pass /dev/null :(
                        std_out = UseHandle terminalHandle,
                        std_err = UseHandle terminalHandle,
                        cwd = Just dir
                      }
              r <- withCreateProcess createProcSpec \_subStdin _noOut _noErr processHandle -> do
                waitForProcess processHandle
                  `onException` ( do
                                    putErrText "Terminating effect process..."
                                    System.Process.withCreateProcess (System.Process.proc runcExe ["kill", name]) \_ _ _ kh ->
                                      waitForProcess kh
                                    threadDelay 3_000_000
                                    System.Process.withCreateProcess (System.Process.proc runcExe ["kill", name, "KILL"]) \_ _ _ kh ->
                                      waitForProcess kh
                                    putErrText "Killed effect process."
                                )
              pure r
          )
          ( do
              masterHandle <- fdToHandle master
              let shovel =
                    handleEOF (BS.hGetLine masterHandle) >>= \case
                      "" -> pass
                      someBytes | "@nix" `BS.isPrefixOf` someBytes -> do
                        -- TODO use it (example @nix { "action": "setPhase", "phase": "effectPhase" })
                        shovel
                      someBytes -> do
                        BS.hPut stderr (someBytes <> "\n")
                        shovel
                  handleEOF = handle \e -> if (ioeGetErrorType e) == HardwareFault then pure "" else throwIO e
              shovel
          )
    pure exitCode
