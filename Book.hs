{-# LANGUAGE TemplateHaskell #-}

module Book where

import           Control.Lens

import           Data.Aeson.TH

import           Data.Text     (Text)


data Book = Book
	{ _title :: Text
	, _authors :: [Text]
	, _isbn :: Text
	}
makeLenses ''Book
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Book)
-- Gives toJson :: Book -> Value
-- 		 fromJson :: Value -> Maybe Book
