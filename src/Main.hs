{-# LANGUAGE FlexibleInstances #-}
module Main where

import Parser
import Evaluator
import qualified Knight.Parse
import qualified Knight.Evaluate

import Data.Maybe
import qualified Data.Map

main = do
  let
    -- Just code = snd $ parse Knight.Parse.value "; = n 10 < n 1"
    Just code = snd $ parse Knight.Parse.value ("\
      \ ; = a 0 \
      \ ; = b 1 \
      \ ; = n 10 \
      \ ; W (> n 1) \
      \   ; = t b \ 
      \   ; = b (+ b a) \
      \   ; = a t \
      \     = n - n 1 \
      \ O b \ 
      \" )
    env = Data.Map.empty :: Env

  eval (Knight.Evaluate.value code) env
  print ()
  -- let (_, value) = print $ eval $ fromMaybe $ snd $ parse value "* 3 + 4 2"
-- main = do
--   print $ parse random "3hi42xs8fhdb35(94-$w:"
--   print $ parse randomBreak "3 hello world !! 5 cut short BREAK 3 hello there kenobi"
