module Main where

import Parser
import Knight.Parse
import Knight.Types

main = print $ runParser value "'; = x 3 O 4"
