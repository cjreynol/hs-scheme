{-|
Module      : Context
Description : The datatype for evaluation context
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Context (
      Eval(Eval)
    , initialContext
    , runEvaluate
    , unEval
    ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Map             (Map, empty)
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
