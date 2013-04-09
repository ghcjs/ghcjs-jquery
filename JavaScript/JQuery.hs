{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, OverloadedStrings #-}

{-
  JQuery bindings, loosely based on fay-jquery
-}

module JavaScript.JQuery where

import           GHCJS.Foreign
import           GHCJS.Types

import           JavaScript.JQuery.Internal

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad

import           Data.Default
import           Data.Maybe
import           Data.Text (Text)

type EventType = Text
type Selector  = Text

data HandlerSettings = HandlerSettings { hsPreventDefault           :: Bool
                                       , hsStopPropagation          :: Bool
                                       , hsStopImmediatePropagation :: Bool
                                       , hsDescendantFilter         :: Maybe Selector
                                       , hsHandlerData              :: Maybe (JSRef ())
                                       }

convertHandlerSettings :: HandlerSettings -> (JSBool, JSBool, JSBool, JSString, JSRef ())
convertHandlerSettings (HandlerSettings pd sp sip ds hd) =
  (toJSBool pd, toJSBool sp, toJSBool sip, maybe jsNull toJSString ds, fromMaybe jsNull hd)

instance Default HandlerSettings where
  def = HandlerSettings False False False Nothing Nothing

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

setHtml :: Text -> JQuery -> IO JQuery
setHtml t = jq_setHtml (toJSString t)

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
selectObject a = jq_selectObject (castRef a)

select :: Text -> IO JQuery
select q = jq_select (toJSString q)

selectEmpty :: IO JQuery
selectEmpty = jq_selectEmpty

-- :: Text -> Either JQuery JSObject -> IO JQuery ?
selectWithContext :: Text -> JSObject a -> IO JQuery
selectWithContext t o = jq_selectWithContext (toJSString t) (castRef o)

getCss :: Text -> JQuery -> IO Text
getCss t jq = fromJSString <$> jq_getCss (toJSString t) jq

setCss :: Text -> Text -> JQuery -> IO JQuery
setCss k v = jq_setCss (toJSString k) (toJSString v)

getHeight :: JQuery -> IO Double
getHeight = jq_getHeight

setHeight :: Double -> JQuery -> IO JQuery
setHeight = jq_setHeight

getWidth :: JQuery -> IO Double
getWidth = jq_getWidth

setWidth :: Double -> JQuery -> IO JQuery
setWidth = jq_setWidth

getInnerHeight :: JQuery -> IO Double
getInnerHeight = jq_getInnerHeight

getInnerWidth :: JQuery -> IO Double
getInnerWidth = jq_getInnerWidth

getOuterHeight :: Bool   -- ^ include margin?
               -> JQuery
               -> IO Double
getOuterHeight b = jq_getOuterHeight (toJSBool b)

getOuterWidth :: Bool    -- ^ include margin?
              -> JQuery
              -> IO Double
getOuterWidth b = jq_getOuterWidth (toJSBool b)

getScrollLeft :: JQuery -> IO Double
getScrollLeft = jq_getScrollLeft

setScrollLeft :: Double -> JQuery -> IO JQuery
setScrollLeft = jq_setScrollLeft

getScrollTop :: JQuery -> IO Double
getScrollTop = jq_getScrollTop

setScrollTop :: Double -> JQuery -> IO JQuery
setScrollTop = jq_setScrollTop

mkListener :: (JSObject (MVar Event) -> JQuery -> IO JQuery)
           -> (Event -> IO ()) -> JQuery -> IO JQuery
mkListener f a jq = do
  mv <- newEmptyMVar
  forkIO (forever $ takeMVar mv >>= a)
  f (mvarRef mv) jq

click :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
click a = on a "click"

click' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
click' a = on' a "click"

dblclick :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
dblclick a = on a "dblclick"

dblclick' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
dblclick' a = on' a "dblclick"

focusin :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
focusin a = on a "focusin"

focusin' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
focusin' a = on' a "focusin"

focusout :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
focusout a = on a "focusout"

focusout' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
focusout' a = on' a "focusout"

hover :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
hover a = on a "hover"

hover' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
hover' a = on' a "hover"

mousedown :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mousedown a = on a "mousdown"

mousedown' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mousedown' a = on' a "mousedown"

mouseenter :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mouseenter a = on a "mouseenter"

mouseenter' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mouseenter' a = on' a "mouseenter"

mouseleave :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mouseleave a = on a "mouseleave"

mouseleave' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mouseleave' a = on' a "mouseleave"

mousemove :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mousemove a = on a "mousemove"

mousemove' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mousemove' a = on' a "mousemove"

mouseout :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mouseout a = on a "mouseout"

mouseout' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mouseout' a = on' a "mouseout"

mouseover :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mouseover a = on a "mouseover"

mouseover' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mouseover' a = on' a "mouseover"

mouseup :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
mouseup a = on a "mouseup"

mouseup' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
mouseup' a = on' a "mouseup"

-- off() TODO how should this be handled?

on :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO JQuery
on a et hs jq = mkListener (\mv -> jq_on mv (toJSString et) ds hd sp sip pd) a jq
                  where
                    (pd, sp, sip, ds, hd) = convertHandlerSettings hs

on' :: MVar Event -> EventType -> HandlerSettings -> JQuery -> IO JQuery
on' mv et hs jq = jq_on (mvarRef mv) (toJSString et) ds hd sp sip pd jq
                    where
                      (pd, sp, sip, ds, hd) = convertHandlerSettings hs

one :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO JQuery
one a et hs jq = mkListener (\mv -> jq_one mv (toJSString et) ds hd sp sip pd) a jq
                   where
                     (pd, sp, sip, ds, hd) = convertHandlerSettings hs

one' :: MVar Event -> EventType -> HandlerSettings -> JQuery -> IO JQuery
one' mv et hs jq = jq_one (mvarRef mv) (toJSString et) ds hd sp sip pd jq 
                     where
                       (pd, sp, sip, ds, hd) = convertHandlerSettings hs

trigger :: EventType -> JQuery -> IO ()
trigger et jq = jq_trigger (toJSString et) jq

triggerHandler :: EventType -> JQuery -> IO ()
triggerHandler et jq = jq_triggerHandler (toJSString et) jq

delegateTarget :: Event -> IO Element
delegateTarget = jq_delegateTarget

isDefaultPrevented :: Event -> IO Bool
isDefaultPrevented e = fromJSBool <$> jq_isDefaultPrevented e

isImmediatePropagationStopped :: Event -> IO Bool
isImmediatePropagationStopped e = fromJSBool <$> jq_isImmediatePropagationStopped e

isPropagationStopped :: Event -> IO Bool
isPropagationStopped e = fromJSBool <$> jq_isPropagationStopped e

namespace :: Event -> IO Text
namespace e = fromJSString <$> jq_namespace e

pageX :: Event -> IO Double
pageX = jq_pageX

pageY :: Event -> IO Double
pageY = jq_pageY

preventDefault :: Event -> IO ()
preventDefault = jq_preventDefault

stopPropagation :: Event -> IO ()
stopPropagation = jq_stopPropagation

stopImmediatePropagation :: Event -> IO ()
stopImmediatePropagation = jq_stopImmediatePropagation

target :: Event -> IO Element
target = jq_target

timeStamp :: Event -> IO Double
timeStamp = jq_timeStamp

eventType :: Event -> IO Text
eventType e = fromJSString <$> jq_eventType e

which :: Event -> IO Int
which = jq_eventWhich

blur :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
blur a = on a "blur"

blur' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
blur' a = on' a "blur"

change :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
change a = on a "change"

change' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
change' a = on' a "change"

onFocus :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
onFocus a = on a "focus"

onFocus' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
onFocus' a = on' a "focus"

focus :: JQuery -> IO JQuery
focus = jq_focus

onSelect :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
onSelect a = on a "select"

select' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
select' a = on' a "select"

submit :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
submit a = on a "submit"

submit' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
submit' a = on' a "submit"

keydown :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
keydown a = on a "keydown"

keydown' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
keydown' a = on' a "keydown"

keyup :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
keyup a = on a "keyup"

keyup' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
keyup' a = on' a "keyup"

keypress :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO JQuery
keypress a = on a "keypress"

keypress' :: MVar Event -> HandlerSettings -> JQuery -> IO JQuery
keypress' a = on' a "keypress"

after :: Text -> JQuery -> IO JQuery
after h jq = jq_after (castRef $ toJSString h) jq

afterJQuery :: JQuery -> JQuery -> IO JQuery
afterJQuery j jq = jq_after (castRef j) jq

afterElem :: Element -> JQuery -> IO JQuery
afterElem e jq = jq_after (castRef e) jq

append :: Text -> JQuery -> IO JQuery
append h jq = jq_append (castRef $ toJSString h) jq

appendJQuery :: JQuery -> JQuery -> IO JQuery
appendJQuery j jq = jq_append (castRef j) jq

appendElem :: Element -> JQuery -> IO JQuery
appendElem e jq = jq_append (castRef e) jq

appendTo :: Text -> JQuery -> IO JQuery
appendTo h jq = jq_appendTo (castRef $ toJSString h) jq

appendToJQuery :: JQuery -> JQuery -> IO JQuery
appendToJQuery j jq = jq_appendTo (castRef j) jq

appendToElem :: Element -> JQuery -> IO JQuery
appendToElem e jq = jq_appendTo (castRef e) jq

before :: Text -> JQuery -> IO JQuery
before h jq = jq_before (castRef $ toJSString h) jq

beforeJQuery :: JQuery -> JQuery -> IO JQuery
beforeJQuery j jq = jq_before (castRef j) jq

beforeElem :: Element -> JQuery -> IO JQuery
beforeElem e jq = jq_before (castRef e) jq

data CloneType = WithoutDataAndEvents
               | WithDataAndEvents
               | DeepWithDataAndEvents

clone :: CloneType -> JQuery -> IO JQuery
clone WithoutDataAndEvents  = jq_clone jsFalse jsFalse
clone WithDataAndEvents     = jq_clone jsTrue  jsFalse
clone DeepWithDataAndEvents = jq_clone jsTrue  jsFalse

detach :: JQuery -> IO JQuery
detach = jq_detach jsNull

detachSelector :: Selector -> JQuery -> IO JQuery
detachSelector s = jq_detach (toJSString s)

empty :: JQuery -> IO JQuery
empty = jq_empty

insertAfter :: Text -> JQuery -> IO JQuery
insertAfter h jq = jq_insertAfter (castRef $ toJSString h) jq

insertAfterJQuery :: JQuery -> JQuery -> IO JQuery
insertAfterJQuery j jq = jq_insertAfter (castRef j) jq

insertAfterElem :: Element -> JQuery -> IO JQuery
insertAfterElem e jq = jq_insertAfter (castRef e) jq

insertBefore :: Text -> JQuery -> IO JQuery
insertBefore h jq = jq_insertBefore (castRef $ toJSString h) jq

insertBeforeJQuery :: JQuery -> JQuery -> IO JQuery
insertBeforeJQuery j jq = jq_insertBefore (castRef j) jq

insertBeforeElem :: Element -> JQuery -> IO JQuery
insertBeforeElem e jq = jq_insertBefore (castRef e) jq

prepend :: Text -> JQuery -> IO JQuery
prepend h jq = jq_prepend (castRef $ toJSString h) jq

prependJQuery :: JQuery -> JQuery -> IO JQuery
prependJQuery j jq = jq_prepend (castRef j) jq

prependElem :: Element -> JQuery -> IO JQuery
prependElem e jq = jq_prepend (castRef e) jq

prependTo :: Text -> JQuery -> IO JQuery
prependTo h jq = jq_prependTo (castRef $ toJSString h) jq

prependToJQuery :: JQuery -> JQuery -> IO JQuery
prependToJQuery j jq = jq_prependTo (castRef j) jq

prependToElem :: Element -> JQuery -> IO JQuery
prependToElem e jq = jq_prependTo (castRef e) jq

remove :: JQuery -> IO JQuery
remove = jq_remove jsNull

removeSelector :: Selector -> JQuery -> IO JQuery
removeSelector s = jq_remove (toJSString s)

replaceAll :: Text -> JQuery -> IO JQuery
replaceAll h jq = jq_replaceAll (castRef $ toJSString h) jq

replaceAllJQuery :: JQuery -> JQuery -> IO JQuery
replaceAllJQuery j jq = jq_replaceAll (castRef j) jq

replaceAllElem :: Element -> JQuery -> IO JQuery
replaceAllElem e jq = jq_replaceAll (castRef e) jq

replaceWith :: Text -> JQuery -> IO JQuery
replaceWith h jq = jq_replaceWith (castRef $ toJSString h) jq

replaceWithJQuery :: JQuery -> JQuery -> IO JQuery
replaceWithJQuery j jq = jq_replaceWith (castRef j) jq

replaceWithElem :: Element -> JQuery -> IO JQuery
replaceWithElem e jq = jq_replaceWith (castRef e) jq

unwrap :: JQuery -> IO JQuery
unwrap = jq_unwrap

wrap :: Text -> JQuery -> IO JQuery
wrap h jq = jq_wrap (castRef $ toJSString h) jq

wrapJQuery :: JQuery -> JQuery -> IO JQuery
wrapJQuery j jq = jq_wrap (castRef j) jq

wrapElem :: Element -> JQuery -> IO JQuery
wrapElem e jq = jq_wrap (castRef e) jq

wrapAll :: Text -> JQuery -> IO JQuery
wrapAll h jq = jq_wrapAll (castRef $ toJSString h) jq

wrapAllJQuery :: JQuery -> JQuery -> IO JQuery
wrapAllJQuery j jq = jq_wrapAll (castRef j) jq

wrapAllElem :: Element -> JQuery -> IO JQuery
wrapAllElem e jq = jq_wrapAll (castRef e) jq

wrapInner :: Text -> JQuery -> IO JQuery
wrapInner h jq = jq_wrapInner (castRef $ toJSString h) jq

wrapInnerJQuery :: JQuery -> JQuery -> IO JQuery
wrapInnerJQuery j jq = jq_wrapInner (castRef j) jq

wrapInnerElem :: Element -> JQuery -> IO JQuery
wrapInnerElem e jq = jq_wrapInner (castRef e) jq

addSelector :: Selector -> JQuery -> IO JQuery
addSelector s jq = jq_add (castRef $ toJSString s) jq

addElement :: Element -> JQuery -> IO JQuery
addElement e jq = jq_add (castRef e) jq

addHtml :: Text -> JQuery -> IO JQuery
addHtml h jq = jq_add (castRef $ toJSString h) jq

add :: JQuery -> JQuery -> IO JQuery
add j jq = jq_add (castRef j) jq

-- addSelectorWithContext :: Selector -> JQuery -> JQuery -> IO JQuery
-- addSelectorWithContext = undefined

andSelf :: JQuery -> IO JQuery
andSelf = jq_andSelf

children :: JQuery -> IO JQuery
children = jq_children jsNull

childrenMatching :: Selector -> JQuery -> IO JQuery
childrenMatching s = jq_children (toJSString s)

closestSelector :: Selector -> JQuery -> IO JQuery
closestSelector s jq = jq_closest (castRef $ toJSString s) jq

-- closestWithContext :: Selector -> Selector -> JQuery -> IO JQuery
-- closestWithContext = undefined

closest :: JQuery -> JQuery -> IO JQuery
closest j jq = jq_closest (castRef j) jq

closestElement :: Element -> JQuery -> IO JQuery
closestElement e jq = jq_closest (castRef e) jq

contents :: JQuery -> IO JQuery
contents = jq_contents

-- This just isn't cool[' Can']'t we all just use map?
-- each :: (Double -> Element -> Fay Bool) -> JQuery -> Fay JQuery
-- each = ffi "%2['each'](%1)"

end :: JQuery -> IO JQuery
end = jq_end

eq :: Int -> JQuery -> IO JQuery
eq = jq_eq

filter :: Selector -> JQuery -> IO JQuery
filter s = jq_filter (castRef $ toJSString s)

filterElement :: Element -> JQuery -> IO JQuery
filterElement e = jq_filter (castRef e)

filterJQuery :: JQuery -> JQuery -> IO JQuery
filterJQuery j = jq_filter (castRef j)

find :: Selector -> JQuery -> IO JQuery
find s = jq_find (castRef $ toJSString s)

findJQuery :: JQuery -> JQuery -> IO JQuery
findJQuery j = jq_find (castRef j)

findElement :: Element -> JQuery -> IO JQuery
findElement e = jq_find (castRef e)

first :: JQuery -> IO JQuery
first = jq_first

has :: Selector -> JQuery -> IO JQuery
has s = jq_has (castRef $ toJSString s)

hasElement :: Element -> JQuery -> IO JQuery
hasElement e = jq_has (castRef e)

is :: Selector -> JQuery -> IO JQuery
is s = jq_is (castRef $ toJSString s)

isJQuery :: JQuery -> JQuery -> IO JQuery
isJQuery j = jq_is (castRef j)

isElement :: Element -> JQuery -> IO JQuery
isElement e = jq_is (castRef e)

last :: JQuery -> IO JQuery
last = jq_last

next :: JQuery -> IO JQuery
next = jq_next jsNull

nextSelector :: Selector -> JQuery -> IO JQuery
nextSelector s = jq_next (toJSString s)

nextAll :: JQuery -> IO JQuery
nextAll = jq_nextAll jsNull

nextAllSelector :: Selector -> JQuery -> IO JQuery
nextAllSelector s = jq_nextAll (toJSString s)

nextUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
nextUntil s mf = jq_nextUntil (castRef $ toJSString s) (maybe jsNull toJSString mf)

nextUntilElement :: Element -> Maybe Selector -> JQuery -> IO JQuery
nextUntilElement e mf = jq_nextUntil (castRef e) (maybe jsNull toJSString mf)

not :: Selector -> JQuery -> IO JQuery
not s = jq_not (castRef $ toJSString s)

notElement :: Element -> JQuery -> IO JQuery
notElement e = jq_not (castRef e)

-- notElements :: [Element] -> JQuery -> IO JQuery
-- notElements = jq_notElements

notJQuery :: JQuery -> JQuery -> IO JQuery
notJQuery j = jq_not (castRef j)

offsetParent :: JQuery -> IO JQuery
offsetParent = jq_offsetParent

parent :: JQuery -> IO JQuery
parent = jq_parent jsNull

parentSelector :: String -> JQuery -> IO JQuery
parentSelector s = jq_parent (toJSString s)

parents :: JQuery -> IO JQuery
parents = jq_parents jsNull

parentsSelector :: Selector -> JQuery -> IO JQuery
parentsSelector s = jq_parents (toJSString s)

parentsUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
parentsUntil s mf = jq_parentsUntil (castRef $ toJSString s) (maybe jsNull (castRef . toJSString) mf)

parentsUntilElement :: Element -> Maybe Selector -> JQuery -> IO JQuery
parentsUntilElement e mf = jq_parentsUntil (castRef e) (maybe jsNull (castRef . toJSString) mf)

prev :: JQuery -> IO JQuery
prev = jq_prev jsNull

prevSelector :: Selector -> JQuery -> IO JQuery
prevSelector s = jq_prev (toJSString s)

prevAll :: JQuery -> IO JQuery
prevAll = jq_prevAll jsNull

prevAllSelector :: String -> JQuery -> IO JQuery
prevAllSelector s = jq_prevAll (toJSString s)

prevUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
prevUntil s mf = jq_prevUntil (castRef $ toJSString s) (maybe jsNull toJSString mf)

prevUntilElement :: Element -> Maybe Selector -> JQuery -> IO JQuery
prevUntilElement e mf = jq_prevUntil (castRef e) (maybe jsNull toJSString mf)

siblings :: JQuery -> IO JQuery
siblings = jq_siblings jsNull

siblingsSelector :: Selector -> JQuery -> IO JQuery
siblingsSelector s = jq_siblings (toJSString s)

slice :: Int -> JQuery -> IO JQuery
slice = jq_slice

sliceFromTo :: Int -> Int -> JQuery -> IO JQuery
sliceFromTo = jq_sliceFromTo

{-
data KeyCode = KeyUp
             | KeyDown
             | KeyLeft
             | KeyRight
             | KeyRet
             | SomeKey Double
  deriving (Show)

onKeycode :: (KeyCode -> Fay Bool) -> JQuery -> Fay JQuery
onKeycode callback el = do
  _onKeycode (\code -> callback (case code of
                                   38 -> KeyUp
                                   40 -> KeyDown
                                   37 -> KeyLeft
                                   39 -> KeyRight
                                   13 -> KeyRet
                                   _  -> SomeKey code))

_onKeycode :: (Double -> Fay Bool) -> JQuery -> Fay JQuery
_onKeycode = ffi "%2['keycode'](%1)"
-}

