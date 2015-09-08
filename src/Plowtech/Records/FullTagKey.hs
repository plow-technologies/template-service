{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plowtech.Records.FullTagKey where

import Control.Applicative hiding (Const(..))
import Control.Lens hiding ((.=), Const(..), Index(..))
import Data.Aeson
import Data.BigEnum
import Data.Master.Template
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Vinyl
import Data.Vinyl.Functor (Compose(..), Const(..))
import GHC.TypeLits

newtype PCSerial = PCSerial { _pcSerial :: Text } deriving (Eq, Read, Show)

instance Ord PCSerial where
  compare (PCSerial serial1) (PCSerial serial2) =
    case compare (T.length serial1) (T.length serial2) of
      EQ -> compare serial1 serial2
      comparison -> comparison

instance MaybeComposite PCSerial where
  type HasComposite PCSerial = Just Char
  maybeComposite = CompositeJust (PCSerial . T.pack) (T.unpack . _pcSerial)

instance BigEnum PCSerial where
  toBigEnum = PCSerial . T.reverse . diagonalizeText
  fromBigEnum = undiagonalizeText . T.reverse . _pcSerial

instance ToJSON PCSerial where
  toJSON (PCSerial serial) = toJSON serial

instance FromJSON PCSerial where
  parseJSON = (PCSerial <$>) . parseJSON

newtype TagName = TagName { _tagName :: Text } deriving (Eq, Read, Show)

instance MaybeComposite TagName where
  type HasComposite TagName = Just Char
  maybeComposite = CompositeJust (TagName . T.pack) (T.unpack . _tagName)

instance Ord TagName where
  compare (TagName tag1) (TagName tag2) =
    case compare (T.length tag1) (T.length tag2) of
      EQ -> compare tag1 tag2
      comparison -> comparison

instance BigEnum TagName where
  toBigEnum = TagName . T.reverse . diagonalizeText
  fromBigEnum = undiagonalizeText . T.reverse . _tagName

instance ToJSON TagName where
  toJSON (TagName name) = toJSON name

instance FromJSON TagName where
  parseJSON = (TagName <$>) . parseJSON

newtype Index = Index { _index :: Text } deriving (Eq, Read, Show)

instance Ord Index where
  compare (Index index1) (Index index2) =
    case compare (T.length index1) (T.length index2) of
      EQ -> compare index1 index2
      comparison -> comparison

instance MaybeComposite Index where
  type HasComposite Index = Just Char
  maybeComposite = CompositeJust (Index . T.pack) (T.unpack . _index)

instance BigEnum Index where
  toBigEnum = Index . T.reverse . diagonalizeText
  fromBigEnum = undiagonalizeText . T.reverse . _index

instance ToJSON Index where
  toJSON (Index index) = toJSON index

instance FromJSON Index where
  parseJSON = (Index <$>) . parseJSON

instance (ToJSON (f (g x))) => ToJSON (Compose f g x) where
  toJSON (Compose x) = toJSON x

instance (FromJSON (f (g x))) => FromJSON (Compose f g x) where
  parseJSON = (Compose <$>) . parseJSON

instance (ToJSON a) => ToJSON (Const a b) where
  toJSON (Const x) = toJSON x

instance (FromJSON a) => FromJSON (Const a b) where
  parseJSON = (Const <$>) . parseJSON

type family FullTagKeyField (field :: Symbol) where
  FullTagKeyField "pc"        = PCSerial
  FullTagKeyField "slave_id"  = Word
  FullTagKeyField "tag_name"  = TagName
  FullTagKeyField "index"     = Index

newtype FullTagKeyAttr (field :: Symbol) = FullTagKeyAttr { _fullTagKeyAttr :: FullTagKeyField field }
deriving instance (Eq (FullTagKeyField field)) => Eq (FullTagKeyAttr field)
deriving instance (Ord (FullTagKeyField field)) => Ord (FullTagKeyAttr field)
deriving instance (Show (FullTagKeyField field)) => Show (FullTagKeyAttr field)
deriving instance (Bounded (FullTagKeyField field)) => Bounded (FullTagKeyAttr field)

instance (MaybeComposite (FullTagKeyField attr)) => MaybeComposite (FullTagKeyAttr attr) where
  type HasComposite (FullTagKeyAttr attr) = HasComposite (FullTagKeyField attr)
  maybeComposite = case (maybeComposite :: CompositeMaybe (FullTagKeyField attr) (HasComposite (FullTagKeyField attr))) of
                     CompositeNothing -> CompositeNothing
                     CompositeJust from to -> CompositeJust (FullTagKeyAttr . from) (to . _fullTagKeyAttr)


instance (BigEnum (FullTagKeyField field)) => BigEnum (FullTagKeyAttr field) where
  fromBigEnum = fromBigEnum . _fullTagKeyAttr
  toBigEnum = FullTagKeyAttr . toBigEnum

instance (ToJSON (FullTagKeyField field)) => ToJSON (FullTagKeyAttr field) where
  toJSON = toJSON . _fullTagKeyAttr

instance (FromJSON (FullTagKeyField field)) => FromJSON (FullTagKeyAttr field) where
  parseJSON value = FullTagKeyAttr <$> parseJSON value

fullTagKeyAttr :: Iso' (FullTagKeyField field) (FullTagKeyAttr field)
fullTagKeyAttr = iso FullTagKeyAttr _fullTagKeyAttr

type FullTagKeyFields = 
  [ "pc"
  , "slave_id"
  , "tag_name"
  , "index"
  ]

type FullTagKey = Rec FullTagKeyAttr FullTagKeyFields

type FullTagKeyTemplate = Rec (TemplatesFor Normalized Disjunction FullTagKeyAttr) FullTagKeyFields

type FullTagKeyValidation = Rec (Const Bool :: Symbol -> *) FullTagKeyFields

type FullTagKeyExamples = Rec (Compose [] FullTagKeyAttr) FullTagKeyFields

diagonalizeText :: Integer -> Text
diagonalizeText n = diagonalizeText' n
  where
    modulus = fromIntegral $ fromEnum (maxBound :: Char) + 1
    diagonalizeText' 0 = ""
    diagonalizeText' n = let char = toEnum $ fromIntegral $ n `mod` modulus
                             rest = n `div` modulus
                         in T.cons char $ diagonalizeText' rest

undiagonalizeText :: Text -> Integer
undiagonalizeText text = sum $
  zipWith 
  (\char position -> (fromIntegral $ fromEnum char) * (modulus ^ position))
  (T.unpack text)
  [0..]
  where
    modulus = fromIntegral $ fromEnum (maxBound :: Char) + 1
