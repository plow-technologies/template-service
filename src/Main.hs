{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Lens
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Master.Template (checkTemplate, Has(), MapCheckResult())
import Data.Master.Examples
import Data.Maybe
import Data.Map (Map())
import Data.Proxy
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Aeson
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles
import SimpleStore
import System.IO (hPutStrLn, stderr)
import Plowtech.Service.Types
import Control.Monad.IO.Class

-- | The WAI application serving the API
fullTagKeyValidator :: SimpleStore [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))] -> Application
fullTagKeyValidator store = 
  serve fullTagKeyValidatorAPI
        (getTemplates store :<|> postTemplate store :<|> getTemplate store :<|> validate store :<|> examples store :<|> serveDirectory "static")

storeMToServant :: StoreM '[] (Either StoreError a) -> EitherT ServantErr IO a
storeMToServant = bimapEitherT storeErrorToServantError id . EitherT . runStoreM
  where
    storeErrorToServantError storeError = ServantErr 500 (show storeError) "" []

retryLock :: StoreM stack (Either StoreError a) -> StoreM stack (Either StoreError a)
retryLock action = action >>= either (\err -> case err of StoreLocked -> retryLock action; _ -> return $ Left err) (return . Right)

getTemplates :: SimpleStore [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))] -> EitherT ServantErr IO [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))]
getTemplates templateStore = storeMToServant $ withReadLock templateStore $ readSimpleStore StoreHere

postTemplate :: SimpleStore [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))] -> NamedJSON (Map Text (Has FullTagKeyTemplateJSON)) -> EitherT ServantErr IO () 
postTemplate templateStore namedTemplateJSON = storeMToServant $ retryLock $ withWriteLock templateStore $ do 
  templates <- readSimpleStore StoreHere
  writeSimpleStore StoreHere $ namedTemplateJSON : templates
  liftIO $ print $ toJSON namedTemplateJSON
  checkpointSimpleStore StoreHere
  return ()

getTemplate :: SimpleStore [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))] -> Text -> EitherT ServantErr IO (Map Text (Has FullTagKeyTemplateJSON))
getTemplate templateStore templateName = do 
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (from namedJSON . rlens (Proxy :: Proxy "name") . from namedAttr)))) $ readSimpleStore StoreHere
  case mTemplate of
    Just template -> return $ template ^. (from namedJSON . rlens (Proxy :: Proxy "record") . from namedAttr)
    Nothing -> left err404
 
validate :: SimpleStore [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))] -> Text -> Map Text FullTagKeyJSON -> EitherT ServantErr IO (Map Text (MapCheckResult FullTagKeyTemplateJSON FullTagKeyJSON FullTagKeyValidationJSON))
validate templateStore templateName candidate = do
  template <- getTemplate templateStore templateName
  return $ checkTemplate template candidate

examples :: SimpleStore [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))] -> Text -> EitherT ServantErr IO (Map Text (Has FullTagKeyExamplesJSON))
examples templateStore templateName = do
  template <- getTemplate templateStore templateName
  return $ generateExamples (Proxy :: Proxy (Map Text FullTagKeyJSON)) template

serverSettings :: Settings
serverSettings =
    setPort 8000
  $ setOnException printAllExceptions
  $ defaultSettings

printAllExceptions :: Maybe Request -> SomeException -> IO ()
printAllExceptions Nothing exception = hPutStrLn stderr "Exception in application:" >> hPutStrLn stderr (show exception) >> hPutStrLn stderr ""
printAllExceptions (Just request) exception = do
  hPutStrLn stderr "Exception in application:"
  hPutStrLn stderr $ show exception
  hPutStrLn stderr "...while handling request:"
  hPutStrLn stderr $ show request
  hPutStrLn stderr ""

-- | Entry point
main :: IO ()
main = do
  Right store <- attemptOpenDefaultSimpleStore "template-store" [] 
  runSettings serverSettings $ fullTagKeyValidator store

