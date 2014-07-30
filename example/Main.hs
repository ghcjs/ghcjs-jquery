{-# LANGUAGE OverloadedStrings #-}

import JavaScript.JQuery

import Control.Monad
import Data.Default
import Data.IORef

import qualified Data.Text as T

main = do
  myClick <- select "<div>click here</div>"
  myCount <- select "<div>0</div>"
  counter <- newIORef (0::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', T.pack $ show c'))
  click (\_ -> void $ getCount >>= flip setText myCount) def myClick
  select "body" >>= appendJQuery myClick >>= appendJQuery myCount
