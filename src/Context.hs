{-|
Module      : Context
Description : The datatype for evaluation context
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Context (
      Eval(Eval)
    , getVal
    , initialContext
    , localRun
    , runEvaluate
    , unEval
    ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, local, reader, runReaderT)
import Data.Map as M        (Map, empty, insert, lookup)
import Data.Text            (Text)

import LispException        (LispException, ThrowsException)
import LispVal              (LispVal)


type EvalContext = Map Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EvalContext ThrowsException a}
    deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadReader EvalContext
    , MonadError LispException
    )


initialContext :: EvalContext
initialContext = empty

runEvaluate :: (LispVal -> Eval LispVal) -> EvalContext -> LispVal 
    -> ThrowsException LispVal
runEvaluate evalFunc ctx val = runReaderT (unEval $ evalFunc val) ctx

getVal :: Text -> Eval (Maybe LispVal)
getVal key = reader $ M.lookup key

localRun :: [(Text, LispVal)] -> Eval LispVal -> Eval LispVal
localRun ctxPairs = local (helper ctxPairs)
    where
        helper :: [(Text, LispVal)] -> EvalContext -> EvalContext
        helper pairs ctx = foldr (\(key, val) m -> insert key val m) ctx pairs
