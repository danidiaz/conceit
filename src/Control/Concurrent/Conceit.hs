{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module Control.Concurrent.Conceit ( 
          Conceit (..)
        , _Conceit
        , _runConceit
        , conceit
        , mapConceit
    ) where

import Data.Bifunctor
import Data.Monoid
import Data.Typeable
import Data.Traversable
import Data.Void
import Control.Applicative 
import Control.Monad
import qualified Control.Monad.Catch as Ex
import Control.Exception 
import Control.Concurrent
import Data.Functor.Bind
import Data.Functor.Plus

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#endif

{-| 
    'Conceit' is very similar to 'Control.Concurrent.Async.Concurrently' from the
@async@ package, but it has an explicit error type @e@.

   The 'Applicative' instance runs two actions concurrently, waits until
they finish, and combines their results. 

   However, if any of the actions fails with @e@ the other action is
   immediately cancelled and the whole computation fails with @e@. 

    To put it another way: 'Conceit' behaves like 'Concurrently' for
    successes and like 'race' for errors.  
-}
newtype Conceit e a = Conceit { runConceit :: IO (Either e a) } deriving Functor

instance Bifunctor Conceit where
  bimap f g (Conceit x) = Conceit $ liftM (bimap f g) x

instance Applicative (Conceit e) where
  pure = Conceit . pure . pure
  Conceit fs <*> Conceit as =
         Conceit $ fmap (fmap (\(f, a) -> f a)) $ conceit fs as

instance Alternative (Conceit e) where
  empty = Conceit $ forever (threadDelay maxBound)
  Conceit as <|> Conceit bs =
    Conceit $ fmap (fmap (either id id)) $ race as bs

instance (Monoid a) => Monoid (Conceit e a) where
   mempty = Conceit . pure . pure $ mempty
   mappend c1 c2 = (<>) <$> c1 <*> c2

-- | `>>` is concurrent.
instance Monad (Conceit e) where
   return = pure
   f >>= k = Conceit $ do
      x <- runConceit f
      case x of 
         Left e -> return $ Left e                      
         Right r -> runConceit $ k r
   f >> k = f *> k

instance MonadPlus (Conceit e) where
   mzero = empty
   mplus = (<|>)

instance MonadIO (Conceit e) where
   liftIO = _Conceit

-- | `<!>` makes its two arguments race against each other.
instance Alt (Conceit e) where
    (<!>) = (<|>)

-- | `zero` is a computation that never finishes.
instance Plus (Conceit e) where
    zero = empty

-- | `>>-` is sequential.
instance Bind (Conceit s) where
    (>>-) = (>>=)

-- | `<.>` is concurrent.
instance Apply (Conceit s) where
    (<.>) = (<*>) 
    (<.) = (<*) 
    (.>) = (*>) 


#if MIN_VERSION_mtl(2, 2, 1)
instance MonadError e (Conceit e) where
     throwError = Conceit . pure . Left
     Conceit c `catchError`  h = 
        Conceit $ runExceptT $ ExceptT c `catchError` (ExceptT . runConceit . h)
#endif

-- | Throws exceptions into IO.
instance Ex.MonadThrow (Conceit e) where
    throwM = Conceit . Ex.throwM

-- | Catches exceptions from IO.
instance Ex.MonadCatch (Conceit e) where
      catch (Conceit m) f = Conceit $ Ex.catch m (runConceit . f)

_Conceit :: IO a -> Conceit e a
_Conceit = Conceit . fmap pure  

_runConceit :: Conceit Void a -> IO a
_runConceit c = either absurd id <$> runConceit c 

{-| 
      Works similarly to 'Control.Concurrent.Async.mapConcurrently' from the
@async@ package, but if any of the computations fails with @e@, the others are
immediately cancelled and the whole computation fails with @e@. 
 -}
mapConceit :: (Traversable t) => (a -> IO (Either e b)) -> t a -> IO (Either e (t b))
mapConceit f = runConceit . sequenceA . fmap (Conceit . f)

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

-- Adapted from the race function from async
race :: IO (Either e a) -> IO (Either e b) -> IO (Either e (Either a b)) 
race left right = conceit' left right collect
  where
    collect m = do
        e <- takeMVar m
        case e of
            Left ex -> throwIO ex
            Right (Right (Right r1)) -> return $ Right $ Right r1
            Right (Right (Left e1)) -> return $ Left e1 
            Right (Left (Right r2)) -> return $ Right $ Left r2 
            Right (Left (Left e2)) -> return $ Left e2

-- Adapted from the concurrently function from async
conceit :: IO (Either e a) -> IO (Either e b) -> IO (Either e (a, b))
conceit left right = conceit' left right (collect [])
  where
    collect [Left (Right a), Right (Right b)] _ = return $ Right (a,b)
    collect [Right (Right b), Left (Right a)] _ = return $ Right (a,b)
    collect (Left (Left ea):_) _ = return $ Left ea
    collect (Right (Left eb):_) _ = return $ Left eb
    collect xs m = do
        e <- takeMVar m
        case e of
            Left ex -> throwIO ex
            Right r -> collect (r:xs) m

-- Verbatim copy of the internal concurrently' function from async
conceit' :: IO a -> IO b
         -> (MVar (Either SomeException (Either a b)) -> IO r)
         -> IO r
conceit' left right collect = do
    done <- newEmptyMVar
    mask $ \restore -> do
        lid <- forkIO $ restore (left >>= putMVar done . Right . Left)
                             `catchAll` (putMVar done . Left)
        rid <- forkIO $ restore (right >>= putMVar done . Right . Right)
                             `catchAll` (putMVar done . Left)
        let stop = killThread lid >> killThread rid
        r <- restore (collect done) `onException` stop
        stop
        return r

