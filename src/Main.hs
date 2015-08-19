{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Master.Template (checkTemplate, Has(), MapCheckResult())
import Data.Maybe
import Data.Map (Map())
import Data.Proxy
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Aeson
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles
import SimpleStore
import Plowtech.Service.Types
import Control.Monad.IO.Class

-- | The WAI application serving the API
onpingTagCombinedValidator :: SimpleStore [NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON))] -> Application
onpingTagCombinedValidator store = 
  serve onpingTagCombinedValidatorAPI
        (getTemplates store :<|> postTemplate store :<|> getTemplate store :<|> validate store :<|> serveDirectory "static")

storeMToServant :: StoreM '[] (Either StoreError a) -> EitherT ServantErr IO a
storeMToServant = bimapEitherT storeErrorToServantError id . EitherT . runStoreM
  where
    storeErrorToServantError storeError = ServantErr 500 (show storeError) "" []

retryLock :: StoreM stack (Either StoreError a) -> StoreM stack (Either StoreError a)
retryLock action = action >>= either (\err -> case err of StoreLocked -> retryLock action; _ -> return $ Left err) (return . Right)

getTemplates :: SimpleStore [NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON))] -> EitherT ServantErr IO [NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON))]
getTemplates templateStore = storeMToServant $ withReadLock templateStore $ readSimpleStore StoreHere

postTemplate :: SimpleStore [NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON))] -> NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON)) -> EitherT ServantErr IO () 
postTemplate templateStore namedTemplateJSON = storeMToServant $ retryLock $ withWriteLock templateStore $ do 
  templates <- readSimpleStore StoreHere
  writeSimpleStore StoreHere $ namedTemplateJSON : templates
  liftIO $ print $ toJSON namedTemplateJSON
  checkpointSimpleStore StoreHere
  return ()

getTemplate :: SimpleStore [NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON))] -> Text -> EitherT ServantErr IO (Map Text (Has OnpingTagCombinedTemplateJSON))
getTemplate templateStore templateName = do 
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (namedJSON . rlens (Proxy :: Proxy "name") . namedAttr)))) $ readSimpleStore StoreHere
  case mTemplate of
    Just template -> return $ template ^. (namedJSON . rlens (Proxy :: Proxy "record") . namedAttr)
    Nothing -> left err404
 
validate :: SimpleStore [NamedJSON (Map Text (Has OnpingTagCombinedTemplateJSON))] -> Text -> Map Text OnpingTagCombinedJSON -> EitherT ServantErr IO (Map Text (MapCheckResult OnpingTagCombinedTemplateJSON OnpingTagCombinedJSON OnpingTagCombinedValidationJSON))
validate templateStore templateName candidate = do
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (namedJSON . rlens (Proxy :: Proxy "name") . namedAttr)))) $ readSimpleStore StoreHere
  template <- case mTemplate of
    Just template -> return $ template ^. (namedJSON . rlens (Proxy :: Proxy "record") . namedAttr)
    Nothing -> left err404
  return $ checkTemplate template candidate
  



-- | Entry point
main :: IO ()
main = do
  Right store <- attemptOpenDefaultSimpleStore "template-store" [] 
  run 8000 $ onpingTagCombinedValidator store

