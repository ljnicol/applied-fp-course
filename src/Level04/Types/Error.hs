{-# LANGUAGE OverloadedStrings #-}

module Level04.Types.Error
  ( Error(..)
  , nonEmptyText
  , gtZeroInt
  ) where

import           Data.Text (Text)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | InvalidId
  -- Add another constructor for our DB error types.
  deriving (Eq, Show)

nonEmptyText :: (Text -> a) -> Error -> Text -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)

gtZeroInt :: (Int -> a) -> Error -> Int -> Either Error a
gtZeroInt c e i
  | i >= 0 = Right (c i)
  | otherwise = Left e
