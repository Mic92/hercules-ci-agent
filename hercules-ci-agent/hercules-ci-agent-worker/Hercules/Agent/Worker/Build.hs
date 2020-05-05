module Hercules.Agent.Worker.Build where

import CNix
import CNix.Internal.Context (Derivation)
import Cachix.Client.Store (Store, queryPathInfo, validPathInfoNarHash, validPathInfoNarSize)
import Conduit
import Foreign (ForeignPtr)
import Hercules.Agent.Worker.Build.Prefetched (buildDerivation)
import qualified Hercules.Agent.Worker.Build.Prefetched as Build
import qualified Hercules.Agent.WorkerProtocol.Command.Build as Command.Build
import Hercules.Agent.WorkerProtocol.Event (Event)
import qualified Hercules.Agent.WorkerProtocol.Event as Event
import qualified Hercules.Agent.WorkerProtocol.Event.BuildResult as Event.BuildResult
import Protolude
import Unsafe.Coerce

runBuild :: Ptr (Ref NixStore) -> Command.Build.Build -> ConduitT i Event (ResourceT IO) ()
runBuild store build = do
  let extraPaths = Command.Build.inputDerivationOutputPaths build
      drvPath = toS $ Command.Build.drvPath build
  for_ extraPaths $ \input ->
    liftIO $ CNix.ensurePath store input
  derivationMaybe <- liftIO $ Build.getDerivation store drvPath
  derivation <- case derivationMaybe of
    Just drv -> pure drv
    Nothing -> panic $ "Could not retrieve derivation " <> show drvPath <> " from local store or binary caches."
  nixBuildResult <- liftIO $ buildDerivation store drvPath derivation (extraPaths <$ guard (not (Command.Build.materializeDerivation build)))
  liftIO $ putErrText $ show nixBuildResult
  buildResult <- liftIO $ enrichResult store derivation nixBuildResult
  yield $ Event.BuildResult buildResult

-- TODO: case distinction on BuildStatus enumeration
enrichResult :: Ptr (Ref NixStore) -> ForeignPtr Derivation -> Build.BuildResult -> IO Event.BuildResult.BuildResult
enrichResult _ _ result@Build.BuildResult {isSuccess = False} = pure $
  Event.BuildResult.BuildFailure {errorMessage = Build.errorMessage result}
enrichResult store derivation _ = do
  drvOuts <- getDerivationOutputs derivation
  outputInfos <- for drvOuts $ \drvOut -> do
    vpi <- queryPathInfo (coerceStore store) (derivationOutputPath drvOut)
    hash_ <- validPathInfoNarHash vpi
    let size = validPathInfoNarSize vpi
    pure Event.BuildResult.OutputInfo
      { name = derivationOutputName drvOut,
        path = derivationOutputPath drvOut,
        hash = hash_,
        size = size
      }
  pure $ Event.BuildResult.BuildSuccess outputInfos

-- TODO factor out cnix library and avoid unsafeCoerce
coerceStore :: Ptr (Ref NixStore) -> Store
coerceStore = unsafeCoerce
