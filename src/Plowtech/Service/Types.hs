{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Plowtech.Service.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad (mzero)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Map (Map())
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Serialize hiding (encode, decode, Get)
import           Data.Text (Text)
import           Data.Vinyl
import           Data.Vinyl.Aeson
import           Data.Vinyl.Lens
import           Data.Vinyl.Functor (Const(..), Identity(..), Compose(..))
import           GHC.TypeLits
import           Plowtech.Records.FullTagKey
import           Servant.API
import           Data.Vinyl.TypeLevel
import           Data.Master.Template
import           Data.Master.Examples (Generatable(..))


-- Orphan instances :\
instance (Eq (f (g x))) => Eq (Compose f g x) where
  (Compose a) == (Compose b) = a == b

instance (Show (f (g x))) => Show (Compose f g x) where
  show (Compose x) = show x

instance (Bounded a) => Bounded (Maybe a) where
  minBound = Nothing
  maxBound = Just maxBound

instance (Enum a) => Enum (Maybe a) where
  toEnum x = if x == 0 then Nothing else Just $ toEnum $ x -1
  fromEnum Nothing = 0
  fromEnum (Just x) = fromEnum x + 1

-- | A newtype for JSON-encoding FullTagKey records
newtype FullTagKeyJSON = FullTagKeyJSON { _fullTagKeyJSON :: FullTagKey }

fullTagKeyJSON :: Iso' FullTagKey FullTagKeyJSON
fullTagKeyJSON = iso FullTagKeyJSON _fullTagKeyJSON

instance Checkable FullTagKeyJSON where
  type Template FullTagKeyJSON = FullTagKeyTemplateJSON
  type Result FullTagKeyJSON = FullTagKeyValidationJSON
  checkTemplate template candidate = checkTemplate (template ^. from fullTagKeyTemplateJSON) (candidate ^. from fullTagKeyJSON) ^. fullTagKeyValidationJSON
  success _ = success (Proxy :: Proxy FullTagKeyValidation) . (^. from fullTagKeyValidationJSON)

newtype FullTagKeyValidationJSON = FullTagKeyValidationJSON { _fullTagKeyValidationJSON :: FullTagKeyValidation }

fullTagKeyValidationJSON :: Iso' FullTagKeyValidation FullTagKeyValidationJSON
fullTagKeyValidationJSON = iso FullTagKeyValidationJSON _fullTagKeyValidationJSON

newtype FullTagKeyExamplesJSON = FullTagKeyExamplesJSON { _fullTagKeyExamplesJSON :: FullTagKeyExamples }
deriving instance (RecAll (Compose [] FullTagKeyAttr) FullTagKeyFields Show) => Show FullTagKeyExamplesJSON

fullTagKeyExamplesJSON :: Iso' FullTagKeyExamples FullTagKeyExamplesJSON
fullTagKeyExamplesJSON = iso FullTagKeyExamplesJSON _fullTagKeyExamplesJSON

instance Generatable FullTagKeyJSON where
  type Examples FullTagKeyJSON = FullTagKeyExamplesJSON
  generateExamples _ template = generateExamples (Proxy :: Proxy FullTagKey) (template ^. from fullTagKeyTemplateJSON) ^. fullTagKeyExamplesJSON

instance FromJSON FullTagKeyExamplesJSON where
  parseJSON = (FullTagKeyExamplesJSON <$>) . recordFromJSON

instance ToJSON FullTagKeyExamplesJSON where
  toJSON = recordToJSON . _fullTagKeyExamplesJSON

-- | Fields for the Named record
type family NamedField a (field :: Symbol) where
  NamedField a "name" = Text
  NamedField a "record" = a

-- | Attribute type for the Named record
newtype NamedAttr a (field :: Symbol) = NamedAttr { _namedAttr :: NamedField a field }
deriving instance (Eq (NamedField a field)) => Eq (NamedAttr a field)

namedAttr :: Iso' (NamedField a field) (NamedAttr a field)
namedAttr = iso NamedAttr _namedAttr

-- | The Named record
type Named a = Rec (NamedAttr a) '["name", "record"]

-- | A newtype for JSON-encoding Named records
newtype NamedJSON a = NamedJSON { _namedJSON :: Named a }
deriving instance (Eq (Named a)) => Eq (NamedJSON a)

namedJSON :: Iso' (Named a) (NamedJSON a)
namedJSON = iso NamedJSON _namedJSON

-- | A newtype for JSON-encoding FullTagKeyTemplate records
newtype FullTagKeyTemplateJSON = FullTagKeyTemplateJSON { _fullTagKeyTemplateJSON :: FullTagKeyTemplate }

fullTagKeyTemplateJSON :: Iso' FullTagKeyTemplate FullTagKeyTemplateJSON
fullTagKeyTemplateJSON = iso FullTagKeyTemplateJSON _fullTagKeyTemplateJSON

instance Eq FullTagKeyTemplateJSON where
  (FullTagKeyTemplateJSON a) == (FullTagKeyTemplateJSON b) = 
      (eqReqOn (Proxy :: Proxy "pc") a b)
   && (eqReqOn (Proxy :: Proxy "slave_id") a b)
   && (eqReqOn (Proxy :: Proxy "tag_name") a b)
   && (eqReqOn (Proxy :: Proxy "index") a b)

eqReqOn :: (RElem r FullTagKeyFields (RIndex r FullTagKeyFields), (Eq (FullTagKeyField r))) 
    => sing r 
    -> Rec (TemplatesFor Normalized Disjunction FullTagKeyAttr) FullTagKeyFields 
    -> Rec (TemplatesFor Normalized Disjunction FullTagKeyAttr) FullTagKeyFields -> Bool
eqReqOn rl template1 template2 = (getCompose $ template1 ^. (rlens rl)) == (getCompose $ template2 ^. (rlens rl))

-- | The Servant API for validation
type FullTagKeyValidatorAPI =
       "templates" :> Get '[JSON] [NamedJSON (Map Text (Has FullTagKeyTemplateJSON))]
  :<|> "templates" :> ReqBody '[JSON] (NamedJSON (Map Text (Has FullTagKeyTemplateJSON))) :> Post '[JSON] ()
  :<|> "templates" :> Capture "name" Text :> Get '[JSON] (Map Text (Has FullTagKeyTemplateJSON))
  :<|> "validate"  :> Capture "name" Text :> ReqBody '[JSON] (Map Text FullTagKeyJSON) :> Post '[JSON] (Map Text (MapCheckResult FullTagKeyTemplateJSON FullTagKeyJSON FullTagKeyValidationJSON))
  :<|> "examples"  :> Capture "name" Text :> Get '[JSON] (Map Text (Has FullTagKeyExamplesJSON))
  :<|>  Raw

instance ToJSON FullTagKeyJSON where
  toJSON = recordToJSON . _fullTagKeyJSON

instance FromJSON FullTagKeyJSON where
  parseJSON = (FullTagKeyJSON <$>) . recordFromJSON

instance (ToJSON a) => ToJSON (NamedJSON a) where
  toJSON = recordToJSON . _namedJSON

instance (FromJSON a) => FromJSON (NamedJSON a) where
  parseJSON = (NamedJSON <$>) . recordFromJSON

instance ToJSON FullTagKeyTemplateJSON where
  toJSON = recordToJSON . _fullTagKeyTemplateJSON

instance FromJSON FullTagKeyTemplateJSON where
  parseJSON = (FullTagKeyTemplateJSON <$>) . recordFromJSON

instance ToJSON FullTagKeyValidationJSON where
  toJSON = recordToJSON . _fullTagKeyValidationJSON

instance FromJSON FullTagKeyValidationJSON where
  parseJSON = (FullTagKeyValidationJSON <$>) . recordFromJSON

instance (ToJSON (NamedField a field)) => ToJSON (NamedAttr a field) where
  toJSON = toJSON . _namedAttr

instance (FromJSON (NamedField a field)) => FromJSON (NamedAttr a field) where
  parseJSON = (NamedAttr <$>) . parseJSON

instance (FromJSON a, ToJSON a) => Serialize (NamedJSON a) where
  put = put . encode
  get = get >>= maybe mzero return . decode

-- | The proxy for the validation Servant API type
fullTagKeyValidatorAPI :: Proxy FullTagKeyValidatorAPI
fullTagKeyValidatorAPI = Proxy
