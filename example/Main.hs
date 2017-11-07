{-# LANGUAGE OverloadedStrings #-}

import JavaScript.JQuery

import Control.Monad
import Data.Default
import Data.IORef
import Data.JSString.Text

import qualified Data.Text as T

main :: IO ()
main = ready $ do
  myClick <- select "<div>click here</div>"
  myCount <- select "<div>0</div>"
  counter <- newIORef (0::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', T.pack $ show c'))
  _ <- click (\_ -> void $ getCount >>= flip (setText . textToJSString) myCount) def myClick
  _ <- select "#counter-area" >>= appendJQuery myClick >>= appendJQuery myCount
  theBtn <- select "#a-button"
  theMsg <- select "#a-message"
  _ <- click (\e -> do
               preventDefault e
               void $ do
                 _ <- setHtml "Clicked!" theMsg
                 _ <- removeClass "btn-primary" theBtn
                 addClass "btn-success" theBtn
        ) def theBtn
  return ()
