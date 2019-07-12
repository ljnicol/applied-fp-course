{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Level05.AppM
  ( AppM
  , liftEither
  , runAppM
  ) where

import           Control.Applicative    (liftA2)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Text              (Text)

import           Level05.Types          (Error)

import           Data.Bifunctor         (first)

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will create a newtype `AppM` that is a shorthand way of
-- describing the return type of a function that may contain an error.
--
-- Our `AppM` type will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value. With the added bonus of allowing us to perform `IO` actions!
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     needsAButMightFail :: Int -> IO (Either Error Value)
-- our type signatures will change, eg
-- foo :: IO (Either Error Value)
-- foo :: AppM Value
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.
newtype AppM a =
  AppM (IO (Either Error a))

-- Everything in this module could easily be replaced by:
-- newtype App a = App
--   { runApp :: ExceptT Error IO a
--   } deriving (Functor, Applicative, Monad, MonadIO, MonadError Error)
--
--
-- This structure allows us to start writing our functions in terms of
-- constraints. As an example, if we wanted to abstract over IO and indicate
-- that instead of the concrete type we wanted a constraint that allows for IO
-- actions. Our AppM would look more like this:
--
-- AppM m a = AppM ( m (Either Error a) )
--
-- Then our functions would look like:
--
-- foo :: MonadIO m => Int -> AppM m a
--
-- Or we could not use a concrete type for Error
--
-- AppM e m a = AppM ( m (Either e a) )
runAppM :: AppM a -> IO (Either Error a)
runAppM (AppM m) = m

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  -- (a -> b) -> IO (Either Error a) -> IO (Either Error b)
  -- I solved this!
  fmap f appMa = AppM $ (fmap . fmap) f (runAppM appMa)
  -- AppM $ fmap (fmap f) (runAppM appMa)
  -- AppM $ fmap f <$> runAppM appMa
  -- using pointfree, fmap . fmap
  -- (.) :: (b -> c) -> (a -> b) -> (a -> c)
  -- function composition
  -- Explanation:
  -- AppM to wrap everything back to an AppM
  -- fmap (fmap f):
  -- fmap f :: Either Error a -> Either Error b
  -- replace f with (fmap f): a === Either Error a, b === Either Error b
  -- fmap (fmap f) :: (Either Error a -> Either Error b) -> IO (Either Error a) -> IO (Either Error b)
  -- leverage existing instances of IO and Either (so we can use fmap).

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM $ (pure . pure) a
  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) appmAtoB appMa = AppM $ liftA2 (<*>) (runAppM appmAtoB) (runAppM appMa)
    -- AppM $ (<*>) <$> (runAppM appmAtoB) <*> (runAppM appMa)
    -- AppM $ (<*>) (fmap (<*>) (runAppM appmAtoB)) (runAppM appMa)
  -- runAppM appmAtoB :: IO (Either Error (a -> b))
  -- fmap :: (a -> b) -> m a -> m b
  -- fmap :: (a -> b) -> IO a -> IO b
  -- where a === Either Error (a -> b) and b === Either Error b
  -- so (a -> b ) === Either Error (a -> b) -> Either Error b
  -- (<*>) :: Either Error (a -> b) -> Either Error a -> Either Error b

instance Monad AppM where
  return :: a -> AppM a
  return = pure
  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  (>>=) appMa atoAppMB =
    AppM $ do
      a <- runAppM appMa
      case a of
        Right a0 -> runAppM $ atoAppMB a0
        Left e   -> pure $ Left e

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO = AppM . fmap pure
  -- liftIO ioA =
    -- AppM $ (>>=) ioA (pure . pure)
  -- equivalent to
    -- AppM $ do
    --   a <- ioA
    --   pure . pure $ a

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError = AppM . pure . Left
  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError appMa handleError =
    AppM $
      -- bind on the IO
     do
      me <- (runAppM appMa)
      either (runAppM . handleError) (pure . Right) me
      -- equivalent to
      -- case me of
      --   Left e  -> runAppM $ handleError e
      --   Right b -> pure $ Right b

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either Error a -> AppM a
liftEither = AppM . pure
-- Go to 'src/Level05/DB.hs' next.
