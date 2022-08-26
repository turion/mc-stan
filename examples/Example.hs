{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE OverloadedStrings #-}

-- text
import qualified Data.Text.IO as Text

-- process
import System.Process

-- mc-stan
import McStan

main :: IO ()
main = do
  printStan model
  Right ((), code) <- runStanT model
  Text.writeFile "model.stan" code
  -- TODO Input should come from the model as well, serialise model to CSV
  output <- readProcess "stanc" ["model.stan"] "x\n23"
  putStrLn output
  output <- readProcess "./model" [] ""
  putStrLn output

model = do
  x <- realD "x" 23
  mu <- realP "mu"
  sigma <- realP "sigma"
  let transformed = mu + sigma
  x <~ Normal mu sigma
  return ()
