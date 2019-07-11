{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Level04.DB
  ( FirstAppDB(FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment, CommentText,
                                                     Error (..), Topic,
                                                     fromDBComment, fromDBTopic,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Control.Monad                      (join)
import           Data.Bifunctor                     (bimap, first)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|
-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB (FirstAppDB c) = Sql.close c

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB :: FilePath -> IO (Either SQLiteResponse FirstAppDB)
initDB fp
  -- open a connection.
  -- creatething
  -- constructor
  -- runDBAction wraps IO a in Either
 =
  Sql.runDBAction $ do
    conn <- Sql.open fp
    -- execute_ returns IO () so throw away result
    _ <- Sql.execute_ conn createTableQ
    -- lift FirstAppDb into IO a
    pure $ FirstAppDB conn
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
  where
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
getComments (FirstAppDB conn) t =
  let sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DBComment
  -- cannot be converted to a Comment, or simply ignoring any DBComment that is
  -- not valid.
   in do dbResponse <-
           Sql.runDBAction $ Sql.query conn sql (Sql.Only (getTopic t)) -- list of DBComment
         let wrappedError = first DatabaseError dbResponse -- Either Error [r0]
         -- traverse fromDBComment has type [DBComment] -> Either Error [Comment]
         -- (>>=) has type m a -> (a -> m b) -> m b
         -- so here m is Either Error
         -- a is [DBComment]
         -- and b is [Comment]
         -- so (>>=) wrappedError (traverse fromDBComment)
         pure $ wrappedError >>= traverse fromDBComment
  --  in do dbResponse <- Sql.query conn sql (Sql.Only (getTopic t))
  --        pure $ traverse fromDBComment dbResponse

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic (FirstAppDB conn) t ct =
  let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
   in do time <- getCurrentTime
         dbResponse <-
           Sql.runDBAction $ do
             Sql.withTransaction conn $
               Sql.execute conn sql (getTopic t, getCommentText ct, time)
         pure $ first DatabaseError dbResponse

getTopics :: FirstAppDB -> IO (Either Error [Topic])
getTopics (FirstAppDB conn) =
  let sql = "SELECT DISTINCT topic FROM comments"
      -- rows <- Sql.query
   in do dbResponse <- Sql.runDBAction $ Sql.query_ conn sql
         let wrappedError = first DatabaseError dbResponse
         pure $ wrappedError >>= traverse fromDBTopic

deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
deleteTopic (FirstAppDB conn) t =
  let sql = "DELETE FROM comments WHERE topic = ?"
   in do dbResponse <-
           Sql.runDBAction $ do
             Sql.withTransaction conn $
               Sql.execute conn sql (Sql.Only (getTopic t))
         pure $ first DatabaseError dbResponse
