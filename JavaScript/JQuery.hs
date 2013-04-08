{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{-
  JQuery bindings, loosely based on fay-jquery
-}

module JavaScript.JQuery where

import           GHCJS.Foreign
import           GHCJS.Types
import           Data.Text (Text)
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent
import           JavaScript.JQuery.Internal
import           Control.Monad
import           Unsafe.Coerce

type EventType = Text
type Selector  = Text

addClass :: Text -> JQuery -> IO JQuery
addClass c = jq_addClass (toJSString c)

getAttr :: Text -> JQuery -> IO Text
getAttr a jq = fromJSString <$> jq_getAttr (toJSString a) jq

setAttr :: Text -> Text -> JQuery -> IO JQuery
setAttr a v = jq_setAttr (toJSString a) (toJSString v)

hasClass :: Text -> JQuery -> IO Bool
hasClass c jq = fromJSBool <$> jq_hasClass (toJSString c) jq

getHtml :: JQuery -> IO Text
getHtml jq = fromJSString <$> jq_getHtml jq

-- setHtml :: Text -> JQuery -> IO JQuery
-- setHtml t = jq_setHtml (toJSString t)

getProp :: Text -> JQuery -> IO Text
getProp p jq = fromJSString <$> jq_getProp (toJSString p) jq

-- fixme value can be Boolean or Number
setProp :: Text -> Text -> JQuery -> IO JQuery
setProp p v = jq_setProp (toJSString p) (toJSString v)

removeAttr :: Text -> JQuery -> IO JQuery
removeAttr a = jq_removeAttr (toJSString a)

removeClass :: Text -> JQuery -> IO JQuery
removeClass c = jq_removeClass (toJSString c)

removeProp :: Text -> JQuery -> IO JQuery
removeProp p = jq_removeProp (toJSString p)

-- toggleClass :: Text -> JQuery -> IO JQuery
-- toggleClass c = jq_toggleClass (toJSString c)

getVal :: JQuery -> IO Text
getVal jq = fromJSString <$> jq_getVal jq

setVal :: Text -> JQuery -> IO JQuery
setVal v = jq_setVal (toJSString v)

getText :: JQuery -> IO Text
getText jq = fromJSString <$> jq_getText jq

setText :: Text -> JQuery -> IO JQuery
setText t = jq_setText (toJSString t)

holdReady :: Bool -> IO ()
holdReady b = jq_holdReady (toJSBool b)

selectElement :: Element -> IO JQuery
selectElement = jq_selectElement

selectObject :: JSObject a -> IO JQuery
selectObject = jq_selectObject

select :: Text -> IO JQuery
select q = jq_select (toJSString q)

selectEmpty :: IO JQuery
selectEmpty = jq_selectEmpty

-- :: Text -> Either JQuery JSObject -> IO JQuery ?
selectWithContext :: Text -> JSObject a -> IO JQuery
selectWithContext t o = jq_selectWithContext (toJSString t) o

----
---- CSS
----

getCss :: Text -> JQuery -> IO Text
getCss t jq = fromJSString <$> jq_getCss (toJSString t) jq

setCss :: Text -> Text -> JQuery -> IO JQuery
setCss k v = jq_setCss (toJSString k) (toJSString v)

getHeight :: JQuery -> IO Double
getHeight = jq_getHeight

setHeight :: Double -> JQuery -> IO JQuery
setHeight d jq = jq_setHeight d jq

getInnerHeight :: JQuery -> IO Double
getInnerHeight = jq_getInnerHeight

getInnerWidth :: JQuery -> IO Double
getInnerWidth = jq_getInnerWidth

getOuterHeight :: JQuery -> IO Double
getOuterHeight = jq_getOuterHeight jsFalse

getOuterHeightMargin :: JQuery -> IO Double
getOuterHeightMargin = jq_getOuterHeight jsTrue

getOuterWidth :: JQuery -> IO Double
getOuterWidth = jq_getOuterWidth jsFalse

getOuterWidthMargin :: JQuery -> IO Double
getOuterWidthMargin = jq_getOuterWidth jsTrue

mkListener :: (JSObject (MVar Event) -> JQuery -> IO JQuery)
           -> (Event -> IO ()) -> JQuery -> IO JQuery
mkListener f a jq = do
  mv <- newEmptyMVar
  forkIO (forever $ readMVar mv >>= a)
  f (unsafeCoerce mv) jq

{-
click :: (Event -> IO ()) -> JQuery -> IO JQuery
click = mkListener jq_click

click' :: MVar Event -> JQuery -> IO JQuery
click' = jq_click

dblclick :: (Event -> IO ()) -> JQuery -> IO JQuery
dblclick = mkListener jq_dblclick

dblclick' :: MVar Event -> JQuery -> IO JQuery
dblclick' = jq_dblclick

focusin :: (Event -> IO ()) -> JQuery -> IO JQuery
focusin = mkListener jq_focusin

focusin' :: MVar Event -> JQuery -> IO JQuery
focusin' = jq_focusin

focusout :: (Event -> IO ()) -> JQuery -> IO JQuery
focusout = mkListener jq_focusout

focusout' :: MVar Event -> JQuery -> IO JQuery
focusout' = jq_focusout

hover :: (Event -> IO ()) -> JQuery -> IO JQuery
hover = mkListener jq_hover

hover' :: MVar Event -> JQuery -> IO JQuery
hover' = jq_hover

mousedown :: (Event -> IO ()) -> JQuery -> IO JQuery
mousedown = mkListener jq_mousedown

mousedown' :: MVar Event -> JQuery -> IO JQuery
mousedown' = jq_mousedown

mouseenter :: (Event -> IO ()) -> JQuery -> IO JQuery
mouseender = mkListener jq_mouseenter

mouseenter' :: MVar Event -> JQuery -> IO JQuery
mouseenter' = jq_mouseenter

mouseleave :: (Event -> IO ()) -> JQuery -> IO JQuery
mouseleave = mkListener jq_mouseleave

mouseleave' :: MVar Event -> JQuery -> IO JQuery
mouseleave' = jq_mouseleave

mousemove :: (Event -> IO ()) -> JQuery -> IO JQuery
mousemove = mkListener jq_mousemove

mousemove' :: MVar Event -> JQuery -> IO JQuery
mousemove' = jq_mousemove

mouseout :: (Event -> IO ()) -> JQuery -> IO JQuery
mouseout = mkListener jq_mouseout

mouseout' :: MVar Event -> JQuery -> IO JQuery
mouseout' = jq_mouseout

mouseover :: (Event -> IO ()) -> JQuery -> IO JQuery
mouseover = mkListener jq_mouseover

mouseover' :: MVar Event -> JQuery -> IO JQuery
mouseover' = jq_mouseover

mouseup :: (Event -> IO ()) -> JQuery -> IO JQuery
mouseup = mkListener jq_mouseup

mouseup' :: MVar Event -> JQuery -> IO JQuery
mouseup' = jq_mouseup
-}

bind :: (Event -> IO ()) -> EventType -> JQuery -> IO JQuery
bind h et jq = mkListener (jq_bind (toJSString et)) h jq

bind' :: MVar Event -> EventType -> JQuery -> IO JQuery
bind' mv et jq = jq_bind (toJSString et) (unsafeCoerce mv) jq
{-
bindPreventBubble :: (Event -> IO ()) -> EventType -> JQuery -> IO ()
bindPreventBubble = mkListener jq_bindPreventBubble

bindPreventBubble' :: MVar Event -> EventType -> JQuery -> IO ()
bindPreventBubble' = jq_bindPreventBubble

-- delegate() superceeded by on()
-- die() deprecated
-- live() deprecated
-- off() TODO how should this be handled?

on :: (Event -> IO ()) -> EventType -> JQuery -> IO ()
on = mkListener jq_on

on' :: MVar Event -> EventType -> JQuery -> IO ()
on' = jq_on

onSelect :: (Event -> IO ()) -> EventType -> Selecttor -> JQuery -> IO ()
onSelect = mkListener jq_onSelect

onSelect' :: MVar Event -> EventType -> Selector -> JQuery -> IO ()
onSelect' = jq_onSelect
-}
-- one :: EventType -> (Event -> Fay ()) -> JQuery -> Fay ()
-- one = ffi "%3['one'](%1, %2)"
{-
trigger :: EventType -> JQuery -> IO ()
trigger et jq = jq_trigger (toJSString et) jq

triggerHandler :: EventType -> JQuery -> IO ()
triggerHandler et jq = jq_triggerHandler (toJSString et) jq
-}


