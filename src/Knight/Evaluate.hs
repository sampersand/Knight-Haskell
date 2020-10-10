{-# LANGUAGE FlexibleInstances, TypeApplications #-}

{- | Parsing Knight `Value`s. -}
module Knight.Evaluate where

import Knight.Types
import qualified Knight.Parse
import Parser
import Evaluator

import System.Process
-- import System.Random
import GHC.IO.Handle
import Control.Monad.IO.Class
import Control.Applicative
import Data.Functor
import Data.Map
import Data.Maybe
import System.Exit
import Debug.Trace

class ValueConvertible a where
  into :: a -> Value
  from :: Value -> EvaluatorM a

  -- = <-- fix dumb sublime text formatting

instance ValueConvertible Bool where
  into = Nullary . Bool

  from (Nullary (Null)) = pure False
  from (Nullary (Bool b)) = pure b
  from (Nullary (Num n)) = pure (n /= 0)
  from (Nullary (Text t)) = pure (not $ Prelude.null t)
  from other = value other >>= from

instance ValueConvertible String where
  into = Nullary . Text

  from (Nullary (Null)) = pure "null"
  from (Nullary (Bool b)) = pure $ show b
  from (Nullary (Num n)) = pure $ show n
  from (Nullary (Text t)) = pure t
  from other = value other >>= from

instance ValueConvertible Integer where
  into = Nullary . Num

  from (Nullary (Null)) = pure 0
  from (Nullary (Bool b)) = pure $ if b then 1 else 0
  from (Nullary (Num n)) = pure n
  from (Nullary (Text t)) = pure $ read t -- this will crash if `t` is invalid.
  from other = value other >>= from


nullValue :: Value
nullValue = Nullary Null

literal :: Literal -> Evaluator
literal (Variable var) = EvaluatorM $ \env ->
  pure (env, fromMaybe nullValue (env !? var))
literal lit = pure $ Nullary lit

unary :: UnaryFn -> Value -> Evaluator
unary Not x = into . not <$> from x
unary FnDef x = pure x
unary Call x = value x >>= value
unary Output x = do
  text <- from @String x
  liftIO $ print text
  pure nullValue
unary Prompt x = do
  text <- from @String x
  liftIO $ print text
  line <- liftIO $ getLine
  pure $ into line
unary Quit x = do
  exitCode <- from x
  liftIO $ exitWith $ case exitCode of
    0 -> ExitSuccess
    other -> ExitFailure $ fromInteger other
unary Eval x = do
  code <- from x
  pure $ fromMaybe nullValue (snd $ parse Knight.Parse.value code)
unary System x = do
  cmd <- from x
  let proc = (shell cmd){std_out=CreatePipe}
  (_, stdout, _, ph) <- liftIO $ createProcess proc
  case stdout of
    Just s -> liftIO $ into <$> hGetContents s
    Nothing -> pure nullValue


binaryFn :: ValueConvertible a => (a -> a -> a) -> Value -> Value -> Evaluator
binaryFn f l r = do
  l' <- from l
  r' <- from r
  pure $ into $ f l' r'

binary :: BinaryFn -> Value -> Value -> Evaluator
-- binary Random start stop = do
--   start' <- from start
--   stop' <- from stop
--   pure $ Nullary $ Num $ randomR (start' stop')
binary While cond body = 
    loop
  where
    loop = do
      cond' <- from (trace ("cond:" ++ show cond) cond)
      if trace ("cond':" ++ show cond') cond'
        then value body *> loop
        else pure $ nullValue
binary Endl lhs rhs = value lhs *> value rhs
binary Assign (Nullary (Variable var)) contents = EvaluatorM $ \env -> do
  (env', contents') <- liftIO $ eval (value contents) env
  pure (insert var contents' env', contents')
binary Add l@(Nullary (Text _)) r = binaryFn @String (++) l r 
binary Add l r = binaryFn @Integer (+) l r
binary Sub l r = binaryFn @Integer (-) l r
binary Mul l r = binaryFn @Integer (*) l r
binary Div l r = binaryFn @Integer div l r
binary Mod l r = binaryFn @Integer mod l r
binary Pow l r = binaryFn @Integer (^) l r
binary Lth l r = binaryFn @Bool (<) l r
binary Gth l r = binaryFn @Bool (>) l r
binary And l r = binaryFn @Bool (&&) l r
binary Or l r = binaryFn @Bool (||) l r
binary bin l r = error ("bad arguments given to " ++
    show bin ++ ": " ++ show l ++ ", " ++ show r)

ternary :: TernaryFn -> Value -> Value -> Value -> Evaluator
ternary If cond true false = do
  cond' <- from cond
  value $ if cond' then true else false

value :: Value -> Evaluator
value (Nullary n) = literal n
value (Unary f x) = unary f x
value (Binary f x y) = binary f x y
value (Ternary f x y z) = ternary f x y z
