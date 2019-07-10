{-# LANGUAGE OverloadedStrings #-}

module Level02.Core
  ( runApp
  , app
  ) where

import           Network.Wai                (Application, Request, Response,
                                             lazyRequestBody, pathInfo,
                                             requestMethod, responseLBS,
                                             strictRequestBody)
import           Network.Wai.Handler.Warp   (run)

import           Network.HTTP.Types         (Status, hContentType, status200,
                                             status400, status404)

import           Network.HTTP.Types.Method  (StdMethod (..), parseMethod)

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSchar

import           Data.Either                (either)

import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)

import           Level02.Types              (ContentType (..), Error (..),
                                             RqType (..), errorToMessage,
                                             mkCommentText, mkTopic,
                                             renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|
-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse status contentType responseText =
  responseLBS
    status
    [("Content-Type", renderContentType contentType)]
    responseText

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest inputText bsComment
  -- (<*>)
  --   (fmap (AddRq) (mkTopic inputText))
  --   (mkCommentText $ lazyByteStringToStrictText bsComment)
 =
  AddRq <$> (mkTopic inputText) <*>
  (mkCommentText $ lazyByteStringToStrictText bsComment)
  -- case (mkTopic inputText, mkCommentText $ lazyByteStringToStrictText bsComment) of
  --   (Right topic, Right comment) -> Right $ AddRq topic comment
  --   (Left topicE, _)             -> Left topicE
  --   (_, Left commentE)           -> Left commentE
  -- using fmap!!!
  -- fmap :: (a -> b) -> f a -> f b
  -- (<*>) :: f (a -> b ) -> f a -> f b
  --
  -- ($) :: (a -> b) -> a -> b
  -- (<$>) :: (a -> b ) -> f a -> f b
  -- (<*>) :: f (a -> b) -> f a -> f b
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
  where
    lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

mkViewRequest :: Text -> Either Error RqType
mkViewRequest inputText = fmap ViewRq (mkTopic inputText)

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------
mkErrorResponse :: Error -> Response
mkErrorResponse e = resp400 PlainText (errorToMessage e)

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO (Either Error RqType)
mkRequest request =
  case (parseMethod . requestMethod $ request, pathInfo request) of
    (Right POST, topic:"add":[]) -> do
      comment <- lazyRequestBody request
      pure $ mkAddRequest topic comment
    (Right GET, topic:"view":[]) -> pure $ mkViewRequest topic
    (Right GET, "list":[]) -> pure $ mkListRequest
    _ -> pure $ Left InvalidRequest
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest (AddRq topic comment) =
  Right $
  resp200
    PlainText
    (LBSchar.pack
       ("Add request not implemented " ++ show topic ++ " " ++ show comment))
handleRequest (ViewRq topic) =
  Right $
  resp200
    PlainText
    (LBSchar.pack $ ("View request not implemented " ++ show topic))
handleRequest ListRq =
  Right $ resp200 PlainText ("List request not implemented")

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app :: Application
app inputRequest respond = do
  request <- mkRequest inputRequest
  -- use bind: (>>=) :: m a -> (a -> m b) -> m b
  -- in this case (m = Either Error): Either Error RqType -> (RqType -> Either Error Response) -> Either Error Response
  let a = request >>= handleRequest
  -- either :: (a -> c) -> (b -> c) -> Either a b -> c
  -- takes care of the case statement pattern match.
  respond $ either mkErrorResponse id a

runApp :: IO ()
runApp = run 3000 app
