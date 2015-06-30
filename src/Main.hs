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
import Data.Master.Template (checkTemplates)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Aeson
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import SimpleStore
import Plowtech.Service.Types

-- | The proxy for the validation Servant API type
onpingTagCombinedValidatorAPI :: Proxy OnpingTagCombinedValidatorAPI
onpingTagCombinedValidatorAPI = Proxy

-- | The WAI application serving the API
onpingTagCombinedValidator :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> Application
onpingTagCombinedValidator store = 
  serve onpingTagCombinedValidatorAPI
        (getTemplates store :<|> postTemplate store :<|> getTemplate store :<|> validate store)

storeMToServant :: StoreM '[] (Either StoreError a) -> EitherT ServantErr IO a
storeMToServant = bimapEitherT storeErrorToServantError id . EitherT . runStoreM
  where
    storeErrorToServantError storeError = ServantErr 500 (show storeError) "" []

retryLock :: StoreM stack (Either StoreError a) -> StoreM stack (Either StoreError a)
retryLock action = action >>= either (\err -> case err of StoreLocked -> retryLock action; _ -> return $ Left err) (return . Right)

getTemplates :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> EitherT ServantErr IO [NamedJSON OnpingTagCombinedTemplateJSON]
getTemplates templateStore = storeMToServant $ withReadLock templateStore $ readSimpleStore StoreHere

postTemplate :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> NamedJSON OnpingTagCombinedTemplateJSON -> EitherT ServantErr IO () 
postTemplate templateStore namedTemplateJSON = storeMToServant $ retryLock $ withWriteLock templateStore $ do 
  templates <- readSimpleStore StoreHere
  writeSimpleStore StoreHere $ namedTemplateJSON : templates
  return ()

getTemplate :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> Text -> EitherT ServantErr IO OnpingTagCombinedTemplateJSON 
getTemplate templateStore templateName = do 
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (namedJSON . rlens (Proxy :: Proxy "name") . namedAttr)))) $ readSimpleStore StoreHere
  case mTemplate of
    Just template -> return $ template ^. (namedJSON . rlens (Proxy :: Proxy "record") . namedAttr)
    Nothing -> left err404
 
validate :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> Text -> OnpingTagCombinedJSON -> EitherT ServantErr IO Bool
validate templateStore templateName candidate = do
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (namedJSON . rlens (Proxy :: Proxy "name") . namedAttr)))) $ readSimpleStore StoreHere
  template <- case mTemplate of
    Just template -> return $ template ^. (namedJSON . rlens (Proxy :: Proxy "record") . namedAttr)
    Nothing -> left err404
  return $ and $ recordToList $ checkTemplates (template ^. onpingTagCombinedTemplateJSON) (candidate ^. onpingTagCombinedJSON)
  



-- | Entry point
main :: IO ()
main = do
  Right store <- attemptOpenDefaultSimpleStore "template-store" [] 
  run 8000 $ onpingTagCombinedValidator store

