module Hercules.CLI.Project where

import qualified Data.Attoparsec.Text as A
import Data.Has (Has)
import Hercules.API (Id)
import Hercules.API.Name (Name (Name))
import Hercules.API.Projects (findProjects)
import qualified Hercules.API.Projects as Projects
import qualified Hercules.API.Projects.CreateUserEffectTokenResponse as CreateUserEffectToken
import Hercules.API.Projects.Project (Project)
import qualified Hercules.API.Projects.Project as Project
import qualified Hercules.API.Repos as Repos
import qualified Hercules.API.Repos.RepoKey as RepoKey
import Hercules.CLI.Client (HerculesClientEnv, HerculesClientToken, projectsClient, reposClient, runHerculesClient, runHerculesClientEither)
import qualified Hercules.CLI.Git as Git
import Hercules.CLI.Options (attoparsecReader, packSome)
import Network.HTTP.Types (Status (Status, statusCode))
import Options.Applicative (bashCompleter, completer, help, long, metavar, option, strOption)
import qualified Options.Applicative as Optparse
import Protolude hiding (option)
import RIO (RIO)
import Servant.Client.Core (ClientError (FailureResponse), ResponseF (responseStatusCode))
import Servant.Client.Core.Response (ResponseF (Response))
import qualified Prelude

data ProjectPath = ProjectPath
  { projectPathSite :: Text,
    projectPathOwner :: Text,
    projectPathProject :: Text
  }

instance Prelude.Show ProjectPath where
  show = s projectPathSite <> const "/" <> s projectPathOwner <> const "/" <> s projectPathProject
    where
      s = (toS .)

projectOption :: Optparse.Parser ProjectPath
projectOption =
  option projectPathReadM $
    long "project" <> metavar "PROJECT" <> help "Project path, e.g. github/my-org/my-project"

nameOption :: Optparse.Parser Text
nameOption = strOption $ long "name" <> metavar "NAME" <> help "Name of the state file"

fileOption :: Optparse.Parser FilePath
fileOption = strOption $ long "file" <> metavar "FILE" <> help "Local path of the state file or - for stdio" <> completer (bashCompleter "file")

projectPathReadM :: Optparse.ReadM ProjectPath
projectPathReadM = attoparsecReader parseProjectPath

parseProjectPath :: A.Parser ProjectPath
parseProjectPath =
  pure ProjectPath
    <*> packSome (A.satisfy (/= '/'))
    <* A.char '/'
    <*> packSome (A.satisfy (/= '/'))
    <* A.char '/'
    <*> packSome (A.satisfy (/= '/'))

getProjectPath :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> RIO r ProjectPath
getProjectPath maybeProjectPathParam =
  case maybeProjectPathParam of
    Nothing -> snd <$> findProjectByCurrentRepo
    Just projectKey -> pure projectKey

getProjectEffectData :: (Has HerculesClientToken r, Has HerculesClientEnv r) => Maybe ProjectPath -> RIO r (ProjectPath, Text)
getProjectEffectData maybeProjectPathParam = do
  (projectIdMaybe, path) <- case maybeProjectPathParam of
    Nothing -> findProjectByCurrentRepo
    Just projectKey -> do
      project <- findProjectByKey projectKey
      pure (Project.id <$> project, projectKey)
  projectId <- case projectIdMaybe of
    Just x -> pure x
    Nothing -> do
      putErrText $ "hci: Could not retrieve project. It may not exist, or you may not have access to it. Project: " <> show path
      liftIO exitFailure
  response <- runHerculesClient (Projects.createUserEffectToken projectsClient projectId)
  let token = CreateUserEffectToken.token response
  pure (path, token)

findProjectByKey :: (Has HerculesClientToken r, Has HerculesClientEnv r) => ProjectPath -> RIO r (Maybe Project.Project)
findProjectByKey path =
  runHerculesClient
    ( Projects.findProjects
        projectsClient
        (Just $ Name $ projectPathSite path)
        (Just $ Name $ projectPathOwner path)
        (Just $ Name $ projectPathProject path)
    )
    <&> head

findProjectByCurrentRepo :: (Has HerculesClientToken r, Has HerculesClientEnv r) => RIO r (Maybe (Id Project), ProjectPath)
findProjectByCurrentRepo = do
  url <- liftIO Git.getUpstreamURL
  rs <- runHerculesClientEither (Repos.parseGitURL reposClient url)
  case rs of
    Left (FailureResponse _req Response {responseStatusCode = Status {statusCode = 404}}) -> do
      putErrText "hci: Repository not recognized by Hercules CI. Make sure you're in the right repository, and if you're running Hercules CI Enterprise, make sure you're using the right HERCULES_CI_API_BASE_URL. Alternatively, use the --project option."
      liftIO exitFailure
    Left e -> throwIO e
    Right r ->
      pure
        ( RepoKey.projectId r,
          ProjectPath
            { projectPathSite = RepoKey.siteName r,
              projectPathOwner = RepoKey.ownerName r,
              projectPathProject = RepoKey.repoName r
            }
        )

findProject :: (Has HerculesClientToken r, Has HerculesClientEnv r) => ProjectPath -> RIO r Project.Project
findProject project = do
  rs <-
    runHerculesClient
      ( findProjects
          projectsClient
          (Just $ Name $ projectPathSite project)
          (Just $ Name $ projectPathOwner project)
          (Just $ Name $ projectPathProject project)
      )
  case rs of
    [] -> do
      putErrText $ "Project not found: " <> show project
      liftIO exitFailure
    [p] -> pure p
    _ -> do
      putErrText $ "Project ambiguous: " <> show project
      liftIO exitFailure
