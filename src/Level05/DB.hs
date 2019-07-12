{-# LANGUAGE OverloadedStrings #-}

module Level05.DB
  ( FirstAppDB(FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Data.Function                      ((&))
import           Level05.AppM                       (AppM, liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB = Sql.close . dbConn

initDB :: FilePath -> IO (Either SQLiteResponse FirstAppDB)
initDB fp =
  Sql.runDBAction $
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
   do
    con <- Sql.open fp
  -- Initialise our one table, if it's not there already
    _ <- Sql.execute_ con createTableQ
    pure $ FirstAppDB con
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
  where
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB :: (a -> Either Error b) -> IO a -> AppM b
runDB transformFn dbQuery
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
 = do
  wrappedDbResponse <- first DBError <$> dbAction
  wrappedDbResponse >>= transformFn & liftEither
  -- same as: & is same as $ with arguments flipped
  -- liftEither $ transformFn =<< wrappedDbResponse
  -- =<< is bind with the arguments flipped
  -- dbResponse <- dbAction
  -- let wrappedError = first DBError dbResponse
  -- liftEither $ (<<=) wrappedError transformFn
  where
    dbAction = liftIO $ Sql.runDBAction dbQuery
  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments :: FirstAppDB -> Topic -> AppM [Comment]
getComments (FirstAppDB conn) t =
  let sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      query = Sql.query conn sql (Sql.Only (getTopic t))
   in runDB (traverse fromDBComment) query
  --       dbResponse <-
  --         Sql.runDBAction $ Sql.query conn sql (Sql.Only (getTopic t))
  --       let wrappedError = first DatabaseError dbResponse
  --       pure $ wrappedError >>= traverse fromDBComment

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> AppM ()
addCommentToTopic (FirstAppDB conn) t ct
  -- let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  --  in do time <- getCurrentTime
  --        dbResponse <-
  --          Sql.runDBAction $ do
  --            Sql.withTransaction conn $
  --              Sql.execute conn sql (getTopic t, getCommentText ct, time)
  --        pure $ first DatabaseError dbResponse
 =
  let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      query time =
        Sql.withTransaction conn $
        Sql.execute conn sql (getTopic t, getCommentText ct, time)
   in do currTime <- liftIO $ getCurrentTime
         runDB pure (query currTime)

getTopics :: FirstAppDB -> AppM [Topic]
getTopics (FirstAppDB conn)
  -- let sql = "SELECT DISTINCT topic FROM comments"
  -- -- rows <- Sql.query
  -- in do dbResponse <- Sql.runDBAction $ Sql.query_ conn sql
  --    let wrappedError = first DatabaseError dbResponse
  --    pure $ wrappedError >>= traverse fromDBTopic
 =
  let sql = "SELECT DISTINCT topic FROM comments"
      query = Sql.query_ conn sql
   in runDB (traverse (mkTopic . Sql.fromOnly)) query

deleteTopic :: FirstAppDB -> Topic -> AppM ()
deleteTopic (FirstAppDB conn) t
  -- let sql = "DELETE FROM comments WHERE topic = ?"
  -- in do dbResponse <-
  --         Sql.runDBAction $ do
  --           Sql.withTransaction conn $
  --             Sql.execute conn sql (Sql.Only (getTopic t))
  --       pure $ first DatabaseError dbResponse
 =
  let sql = "DELETE FROM comments WHERE topic = ?"
      query =
        Sql.withTransaction conn $ Sql.execute conn sql (Sql.Only (getTopic t))
   in runDB pure query
  -- Go to 'src/Level05/Core.hs' next.
