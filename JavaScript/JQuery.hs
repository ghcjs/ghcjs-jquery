{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{-
  JQuery bindings, loosely based on fay-jquery
-}

module JavaScript.JQuery ( JQuery(..)
                         , Event(..)
                         , EventType
                         , Selector
                         , Method(..)
                         , AjaxSettings(..)
                         , AjaxResult(..)
                         , ajax
                         , HandlerSettings(..)
                         , ready
                         , addClass
                         , animate
                         , getAttr
                         , setAttr
                         , hasClass
                         , getHtml
                         , setHtml
                         , getProp
                         , setProp
                         , removeAttr
                         , removeClass
                         , removeProp
                         , getVal
                         , setVal
                         , getText
                         , setText
                         , holdReady
                         , selectElement
                         , selectObject
                         , select
                         , selectEmpty
                         , selectWithContext
                         , getCss
                         , setCss
                         , getHeight
                         , setHeight
                         , getWidth
                         , setWidth
                         , getInnerHeight
                         , getOuterHeight
                         , getInnerWidth
                         , getOuterWidth
                         , getScrollLeft
                         , setScrollLeft
                         , getScrollTop
                         , setScrollTop
                         , click
                         , dblclick
                         , focusin
                         , focusout
                         , hover
                         , mousedown
                         , mouseenter
                         , mouseleave
                         , mousemove
                         , mouseout
                         , mouseover
                         , mouseup
                         , on
                         , one
                         , trigger
                         , triggerHandler
                         , delegateTarget
                         , isDefaultPrevented
                         , isImmediatePropagationStopped
                         , isPropagationStopped
                         , namespace
                         , pageX
                         , pageY
                         , preventDefault
                         , stopPropagation
                         , stopImmediatePropagation
                         , target
                         , timeStamp
                         , eventType
                         , which
                         , blur
                         , change
                         , onFocus
                         , focus
                         , onSelect
                         , onSubmit
                         , keydown
                         , keyup
                         , keypress
                         , after
                         , afterJQuery
                         , afterElem
                         , append
                         , appendJQuery
                         , appendElem
                         , appendTo
                         , appendToJQuery
                         , appendToElem
                         , before
                         , beforeJQuery
                         , beforeElem
                         , CloneType(..)
                         , clone
                         , detach
                         , detachSelector
                         , empty
                         , insertAfter
                         , insertAfterJQuery
                         , insertAfterElem
                         , insertBefore
                         , insertBeforeJQuery
                         , insertBeforeElem
                         , prepend
                         , prependJQuery
                         , prependElem
                         , prependTo
                         , prependToJQuery
                         , prependToElem
                         , remove
                         , removeSelector
                         , replaceAll
                         , replaceAllJQuery
                         , replaceAllElem
                         , replaceWith
                         , replaceWithJQuery
                         , replaceWithElem
                         , unwrap
                         , wrap
                         , wrapJQuery
                         , wrapElem
                         , wrapAll
                         , wrapAllJQuery
                         , wrapAllElem
                         , wrapInner
                         , wrapInnerJQuery
                         , wrapInnerElem
                         , addSelector
                         , addElement
                         , addHtml
                         , add
                         , andSelf
                         , children
                         , childrenMatching
                         , closestSelector
                         , closest
                         , closestElement
                         , contents
                         , end
                         , eq
                         , filter
                         , filterElement
                         , filterJQuery
                         , find
                         , findJQuery
                         , findElement
                         , first
                         , has
                         , hasElement
                         , is
                         , isJQuery
                         , isElement
                         , last
                         , next
                         , nextSelector
                         , nextAll
                         , nextAllSelector
                         , nextUntil
                         , nextUntilElement
                         , not
                         , notElement
                         , notJQuery
                         , offsetParent
                         , parent
                         , parentSelector
                         , parents
                         , parentsSelector
                         , parentsUntil
                         , parentsUntilElement
                         , prev
                         , prevSelector
                         , prevAll
                         , prevAllSelector
                         , prevUntil
                         , prevUntilElement
                         , siblings
                         , siblingsSelector
                         , slice
                         , sliceFromTo
                         , stop

                         ) where

import           Prelude hiding (filter, not, last)

import           GHCJS.Marshal
import           GHCJS.Foreign (toJSBool, jsNull)
import           GHCJS.Foreign.Callback as Cb
import           JavaScript.Object.Internal as Obj (Object, create, getProp, setProp)
import           GHCJS.Types
import           GHCJS.DOM.Types (Element(..), IsElement, toElement
                                 , unElement)

import           JavaScript.JQuery.Internal

import           Control.Monad

import           Data.Default
import           Data.Maybe
import           Data.JSString as S (pack)
import           Data.JSString.Text as S (textToJSString)
import           Data.Text as Text (Text)
import           Data.Typeable

default (JSString)

type EventType = Text
type Selector  = Text

data Method = GET | POST | PUT | DELETE deriving (Eq, Ord, Enum, Show)

data AjaxSettings = AjaxSettings { asContentType :: Text
                                 , asCache       :: Bool
                                 , asIfModified  :: Bool
                                 , asMethod      :: Method
                                 } deriving (Ord, Eq, Show, Typeable)

data AjaxResult = AjaxResult { arStatus :: Int
                             , arData   :: Maybe Text
                             } deriving (Ord, Eq, Show, Typeable)

instance Default AjaxSettings where
  def = AjaxSettings "application/x-www-form-urlencoded; charset=UTF-8" True False GET

instance ToJSVal AjaxSettings where
  toJSVal o = fmap jsval (ajaxSettingsToObject o)

ajaxSettingsToObject :: AjaxSettings -> IO Object
ajaxSettingsToObject (AjaxSettings ct cache ifMod method) = do
    o <- Obj.create
    let (.=) :: JSString -> JSVal -> IO ()
        p .= v = Obj.setProp p v o
    "method"      .= (jsval . S.pack . show) method
    "ifModified"  .= toJSBool ifMod
    "cache"       .= toJSBool cache
    "contentType" .= (jsval . textToJSString) ct
    "dataType"    .= jsval ("text" :: JSString)
    return o

ajax :: Text -> [(Text,Text)] -> AjaxSettings -> IO AjaxResult
ajax url d s = do
  o <- Obj.create
  forM_ d (\(k,v) -> Obj.setProp (textToJSString k) ((jsval . textToJSString) v) o)
  os <- ajaxSettingsToObject s
  Obj.setProp ("data"::JSString) (jsval o) os
  res <- jq_ajax (textToJSString url) (jsval os)
  dat <- Obj.getProp ("data"::JSString) res
  md <- fromJSVal dat
  status <- fromMaybe 0 <$> (fromJSVal =<< Obj.getProp "status" res)
  return (AjaxResult status md)

data HandlerSettings = HandlerSettings { hsPreventDefault           :: Bool
                                       , hsStopPropagation          :: Bool
                                       , hsStopImmediatePropagation :: Bool
                                       , hsSynchronous              :: Bool
                                       , hsDescendantFilter         :: Maybe Selector
                                       , hsHandlerData              :: Maybe JSVal
                                       }

convertHandlerSettings :: HandlerSettings -> (Bool, Bool, Bool, JSString, JSVal)
convertHandlerSettings (HandlerSettings pd sp sip _ ds hd) =
  (pd, sp, sip, maybe ("" :: JSString) textToJSString ds, fromMaybe jsNull hd)

instance Default HandlerSettings where
  def = HandlerSettings False False False True Nothing Nothing

ready :: IO () -> IO ()
ready action = do
  clbk <- asyncCallback action
  jq_ready clbk

addClass :: JSString -> JQuery -> IO JQuery
addClass c = jq_addClass c

animate :: Object -> Object -> JQuery -> IO JQuery
animate s t = jq_animate (jsval s) (jsval t)

getAttr :: JSString -> JQuery -> IO JSString
getAttr a jq = jq_getAttr a jq

setAttr :: JSString -> JSString -> JQuery -> IO JQuery
setAttr a v = jq_setAttr a v

hasClass :: JSString -> JQuery -> IO Bool
hasClass c jq = jq_hasClass c jq

getHtml :: JQuery -> IO JSString
getHtml jq = jq_getHtml jq

setHtml :: JSString -> JQuery -> IO JQuery
setHtml t = jq_setHtml t

-- getProp :: JSString -> JQuery -> IO JSString
-- getProp p jq = jq_getProp p jq

-- fixme value can be Boolean or Number
-- setProp :: JSString -> JSString -> JQuery -> IO JQuery
-- setProp p v = jq_setProp p v

removeAttr :: JSString -> JQuery -> IO JQuery
removeAttr a = jq_removeAttr a

removeClass :: JSString -> JQuery -> IO JQuery
removeClass c = jq_removeClass c

removeProp :: JSString -> JQuery -> IO JQuery
removeProp p = jq_removeProp p

-- toggleClass :: JSString -> JQuery -> IO JQuery
-- toggleClass c = jq_toggleClass (S.pack c)

getVal :: JQuery -> IO JSString
getVal jq = jq_getVal jq

setVal :: JSString -> JQuery -> IO JQuery
setVal v = jq_setVal v

getText :: JQuery -> IO JSString
getText jq = jq_getText jq

setText :: JSString -> JQuery -> IO JQuery
setText t = jq_setText t

holdReady :: Bool -> IO ()
holdReady b = jq_holdReady b

selectElement :: IsElement e => e -> IO JQuery
selectElement e = jq_selectElement (toElement e)

selectObject :: Object -> IO JQuery
selectObject a = jq_selectObject (jsval a)

select :: JSString -> IO JQuery
select q = jq_select q

selectEmpty :: IO JQuery
selectEmpty = jq_selectEmpty

-- :: Text -> Either JQuery Object -> IO JQuery ?
selectWithContext :: JSString -> Object -> IO JQuery
selectWithContext t o = jq_selectWithContext t (jsval o)

getCss :: JSString -> JQuery -> IO JSString
getCss t jq = jq_getCss t jq

setCss :: JSString -> JSString -> JQuery -> IO JQuery
setCss k v = jq_setCss k v

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
getOuterHeight b = jq_getOuterHeight b

getOuterWidth :: Bool    -- ^ include margin?
              -> JQuery
              -> IO Double
getOuterWidth b = jq_getOuterWidth b

getScrollLeft :: JQuery -> IO Double
getScrollLeft = jq_getScrollLeft

setScrollLeft :: Double -> JQuery -> IO JQuery
setScrollLeft = jq_setScrollLeft

getScrollTop :: JQuery -> IO Double
getScrollTop = jq_getScrollTop

setScrollTop :: Double -> JQuery -> IO JQuery
setScrollTop = jq_setScrollTop

click :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
click a = on a "click"

dblclick :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
dblclick a = on a "dblclick"

focusin :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
focusin a = on a "focusin"

focusout :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
focusout a = on a "focusout"

hover :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
hover a = on a "hover"

mousedown :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mousedown a = on a "mousedown"

mouseenter :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseenter a = on a "mouseenter"

mouseleave :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseleave a = on a "mouseleave"

mousemove :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mousemove a = on a "mousemove"

mouseout :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseout a = on a "mouseout"

mouseover :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseover a = on a "mouseover"

mouseup :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
mouseup a = on a "mouseup"

{- |  Register an event handler. Use the returned IO action to remove the
      handler.

      Note that the handler will stay in memory until the returned IO action is
      executed, even if the DOM nodes are removed.
 -}
on :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
on a et hs jq = do
  cb <- if hsSynchronous hs
          then Cb.syncCallback1 ContinueAsync (a . Event)
          else Cb.asyncCallback1 (a . Event)
  jq_on cb et' ds hd sp sip pd jq
  return (jq_off cb et' ds jq >> Cb.releaseCallback cb)
    where
      et'                   = textToJSString et
      (pd, sp, sip, ds, hd) = convertHandlerSettings hs

one :: (Event -> IO ()) -> EventType -> HandlerSettings -> JQuery -> IO (IO ())
one a et hs jq = do
  cb <- if hsSynchronous hs
          then Cb.syncCallback1 ContinueAsync (a . Event)
          else Cb.asyncCallback1 (a . Event)
  jq_one cb et' ds hd sp sip pd jq
  return (jq_off cb et' ds jq >> Cb.releaseCallback cb)
    where
      et'                   = textToJSString et
      (pd, sp, sip, ds, hd) = convertHandlerSettings hs

trigger :: EventType -> JQuery -> IO ()
trigger et jq = jq_trigger (textToJSString et) jq

triggerHandler :: EventType -> JQuery -> IO ()
triggerHandler et jq = jq_triggerHandler (textToJSString et) jq

delegateTarget :: Event -> IO Element
delegateTarget ev = Element <$> jq_delegateTarget ev

isDefaultPrevented :: Event -> IO Bool
isDefaultPrevented e = jq_isDefaultPrevented e

isImmediatePropagationStopped :: Event -> IO Bool
isImmediatePropagationStopped e = jq_isImmediatePropagationStopped e

isPropagationStopped :: Event -> IO Bool
isPropagationStopped e = jq_isPropagationStopped e

namespace :: Event -> IO JSString
namespace e = jq_namespace e

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
target ev = Element <$> jq_target ev

timeStamp :: Event -> IO Double
timeStamp = jq_timeStamp

eventType :: Event -> IO JSString
eventType e = jq_eventType e

which :: Event -> IO Int
which = jq_eventWhich

blur :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
blur a = on a "blur"

change :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
change a = on a "change"

onFocus :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
onFocus a = on a "focus"

focus :: JQuery -> IO JQuery
focus = jq_focus

onSelect :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
onSelect a = on a "select"

onSubmit :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
onSubmit a = on a "submit"

keydown :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
keydown a = on a "keydown"

keyup :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
keyup a = on a "keyup"

keypress :: (Event -> IO ()) -> HandlerSettings -> JQuery -> IO (IO ())
keypress a = on a "keypress"

after :: JSString -> JQuery -> IO JQuery
after = jq_after

afterJQuery :: JQuery -> JQuery -> IO JQuery
afterJQuery = jq_after_jq

afterElem :: IsElement e => e -> JQuery -> IO JQuery
afterElem e jq = jq_after_jq (JQuery . unElement $ toElement e) jq

append :: JSString -> JQuery -> IO JQuery
append h jq = jq_append h jq

appendJQuery :: JQuery -> JQuery -> IO JQuery
appendJQuery j jq = jq_append_jq j jq

appendElem :: IsElement e => e -> JQuery -> IO JQuery
appendElem e jq = jq_append_jq (JQuery . unElement $ toElement e) jq

appendTo :: JSString -> JQuery -> IO JQuery
appendTo h jq = jq_appendTo h jq

appendToJQuery :: JQuery -> JQuery -> IO JQuery
appendToJQuery j jq = jq_appendTo_jq j jq

appendToElem :: IsElement e => e -> JQuery -> IO JQuery
appendToElem e jq = jq_appendTo_jq (JQuery . unElement $ toElement e) jq

before :: JSString -> JQuery -> IO JQuery
before h jq = jq_before h jq

beforeJQuery :: JQuery -> JQuery -> IO JQuery
beforeJQuery j jq = jq_before_jq j jq

beforeElem :: IsElement e => e -> JQuery -> IO JQuery
beforeElem e jq = jq_before_jq (JQuery . unElement $ toElement e) jq

data CloneType = WithoutDataAndEvents
               | WithDataAndEvents
               | DeepWithDataAndEvents

clone :: CloneType -> JQuery -> IO JQuery
clone WithoutDataAndEvents  = jq_clone False False
clone WithDataAndEvents     = jq_clone True  False
clone DeepWithDataAndEvents = jq_clone True  True

detach :: JQuery -> IO JQuery
detach = jq_detach

detachSelector :: Selector -> JQuery -> IO JQuery
detachSelector s = jq_detachSelector (textToJSString s)

empty :: JQuery -> IO JQuery
empty = jq_empty

insertAfter :: JSString -> JQuery -> IO JQuery
insertAfter = jq_insertAfter

insertAfterJQuery :: JQuery -> JQuery -> IO JQuery
insertAfterJQuery = jq_insertAfter_jq

insertAfterElem :: IsElement e => e -> JQuery -> IO JQuery
insertAfterElem e jq = jq_insertAfter_jq (JQuery . unElement $ toElement e) jq

insertBefore :: JSString -> JQuery -> IO JQuery
insertBefore = jq_insertBefore

insertBeforeJQuery :: JQuery -> JQuery -> IO JQuery
insertBeforeJQuery = jq_insertBefore_jq

insertBeforeElem :: IsElement e => e -> JQuery -> IO JQuery
insertBeforeElem e jq = jq_insertBefore_jq (JQuery . unElement $ toElement e) jq

prepend :: JSString -> JQuery -> IO JQuery
prepend = jq_prepend

prependJQuery :: JQuery -> JQuery -> IO JQuery
prependJQuery = jq_prepend_jq

prependElem :: IsElement e => e -> JQuery -> IO JQuery
prependElem e jq = jq_prepend_jq (JQuery . unElement $ toElement e) jq

prependTo :: JSString -> JQuery -> IO JQuery
prependTo = jq_prependTo

prependToJQuery :: JQuery -> JQuery -> IO JQuery
prependToJQuery = jq_prependTo_jq

prependToElem :: IsElement e => e -> JQuery -> IO JQuery
prependToElem e jq = jq_prependTo_jq (JQuery . unElement $ toElement e) jq

remove :: JQuery -> IO JQuery
remove = jq_remove

removeSelector :: Selector -> JQuery -> IO JQuery
removeSelector s = jq_removeSelector (textToJSString s)

replaceAll :: JSString -> JQuery -> IO JQuery
replaceAll h jq = jq_replaceAll h jq

replaceAllJQuery :: JQuery -> JQuery -> IO JQuery
replaceAllJQuery = jq_replaceAll_jq

replaceAllElem :: IsElement e => e -> JQuery -> IO JQuery
replaceAllElem e jq = jq_replaceAll_jq (JQuery . unElement $ toElement e) jq

replaceWith :: JSString -> JQuery -> IO JQuery
replaceWith = jq_replaceWith

replaceWithJQuery :: JQuery -> JQuery -> IO JQuery
replaceWithJQuery = jq_replaceWith_jq

replaceWithElem :: IsElement e => e -> JQuery -> IO JQuery
replaceWithElem e jq = jq_replaceWith_jq (JQuery . unElement $ toElement e) jq

unwrap :: JQuery -> IO JQuery
unwrap = jq_unwrap

wrap :: JSString -> JQuery -> IO JQuery
wrap = jq_wrap

wrapJQuery :: JQuery -> JQuery -> IO JQuery
wrapJQuery = jq_wrap_jq

wrapElem :: IsElement e => e -> JQuery -> IO JQuery
wrapElem e jq = jq_wrap_jq (JQuery . unElement $ toElement e) jq

wrapAll :: Selector -> JQuery -> IO JQuery
wrapAll s = jq_wrapAll (textToJSString s)

wrapAllJQuery :: JQuery -> JQuery -> IO JQuery
wrapAllJQuery = jq_wrapAll_jq

wrapAllElem :: IsElement e => e -> JQuery -> IO JQuery
wrapAllElem e jq = jq_wrapAll_jq (JQuery . unElement $ toElement e) jq

wrapInner :: JSString -> JQuery -> IO JQuery
wrapInner = jq_wrapInner

wrapInnerJQuery :: JQuery -> JQuery -> IO JQuery
wrapInnerJQuery = jq_wrapInner_jq

wrapInnerElem :: IsElement e => e -> JQuery -> IO JQuery
wrapInnerElem e jq = jq_wrapInner_jq (JQuery . unElement $ toElement e) jq

addSelector :: Selector -> JQuery -> IO JQuery
addSelector s = jq_add (textToJSString s)

addElement :: IsElement e => e -> JQuery -> IO JQuery
addElement e jq = jq_add_jq (JQuery . unElement $ toElement e) jq

addHtml :: JSString -> JQuery -> IO JQuery
addHtml = jq_add

add :: JQuery -> JQuery -> IO JQuery
add = jq_add_jq

-- addSelectorWithContext :: Selector -> JQuery -> JQuery -> IO JQuery
-- addSelectorWithContext = undefined

andSelf :: JQuery -> IO JQuery
andSelf = jq_andSelf

children :: JQuery -> IO JQuery
children = jq_children ("" :: JSString)

childrenMatching :: Selector -> JQuery -> IO JQuery
childrenMatching s = jq_children (textToJSString s)

closestSelector :: Selector -> JQuery -> IO JQuery
closestSelector s = jq_closest (textToJSString s)

-- closestWithContext :: Selector -> Selector -> JQuery -> IO JQuery
-- closestWithContext = undefined

closest :: JQuery -> JQuery -> IO JQuery
closest = jq_closest_jq

closestElement :: IsElement e => e -> JQuery -> IO JQuery
closestElement e jq = jq_closest_jq (JQuery . unElement $ toElement e) jq

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
filter s = jq_filter (textToJSString s)

filterJQuery :: JQuery -> JQuery -> IO JQuery
filterJQuery = jq_filter_jq

filterElement :: IsElement e => e -> JQuery -> IO JQuery
filterElement e = jq_filter_jq (JQuery . unElement $ toElement e)

find :: Selector -> JQuery -> IO JQuery
find s = jq_find (textToJSString s)

findJQuery :: JQuery -> JQuery -> IO JQuery
findJQuery = jq_find_jq

findElement :: IsElement e => e -> JQuery -> IO JQuery
findElement e = jq_find_jq (JQuery . unElement $ toElement e)

first :: JQuery -> IO JQuery
first = jq_first

has :: Selector -> JQuery -> IO JQuery
has s = jq_has (textToJSString s)

hasElement :: IsElement e => e -> JQuery -> IO JQuery
hasElement e = jq_has_jq (JQuery . unElement $ toElement e)

is :: Selector -> JQuery -> IO Bool
is s = jq_is  (textToJSString s)

isJQuery :: JQuery -> JQuery -> IO Bool
isJQuery = jq_is_jq

isElement :: IsElement e => e -> JQuery -> IO Bool
isElement e = jq_is_jq (JQuery . unElement $ toElement e)

last :: JQuery -> IO JQuery
last = jq_last

next :: JQuery -> IO JQuery
next = jq_next

nextSelector :: Selector -> JQuery -> IO JQuery
nextSelector s = jq_nextSelector (textToJSString s)

nextAll :: JQuery -> IO JQuery
nextAll = jq_nextAll

nextAllSelector :: Selector -> JQuery -> IO JQuery
nextAllSelector s = jq_nextAllSelector (textToJSString s)

nextUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
nextUntil s mf = jq_nextUntil (textToJSString s) (maybe "" textToJSString mf)

nextUntilElement :: IsElement e => e -> Maybe Selector -> JQuery -> IO JQuery
nextUntilElement e mf = jq_nextUntil_jq (JQuery . unElement $ toElement e) (maybe "" textToJSString mf)

not :: Selector -> JQuery -> IO JQuery
not s = jq_not (textToJSString s)

notJQuery :: JQuery -> JQuery -> IO JQuery
notJQuery = jq_not_jq

notElement :: IsElement e => e -> JQuery -> IO JQuery
notElement e = jq_not_jq (JQuery . unElement $ toElement e)

-- notElements :: [Element] -> JQuery -> IO JQuery
-- notElements = jq_notElements

offsetParent :: JQuery -> IO JQuery
offsetParent = jq_offsetParent

parent :: JQuery -> IO JQuery
parent = jq_parent ("" :: JSString)

parentSelector :: Selector -> JQuery -> IO JQuery
parentSelector s = jq_parent (textToJSString s)

parents :: JQuery -> IO JQuery
parents = jq_parents ("" :: JSString)

parentsSelector :: Selector -> JQuery -> IO JQuery
parentsSelector s = jq_parents (textToJSString s)

parentsUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
parentsUntil s mf = jq_parentsUntil (textToJSString s) (maybe "" textToJSString mf)

parentsUntilElement :: IsElement e => e -> Maybe Selector -> JQuery -> IO JQuery
parentsUntilElement e mf = jq_parentsUntil_jq (JQuery . unElement $ toElement e) (maybe "" textToJSString mf)

prev :: JQuery -> IO JQuery
prev = jq_prev ("" :: JSString)

prevSelector :: Selector -> JQuery -> IO JQuery
prevSelector s = jq_prev (textToJSString s)

prevAll :: JQuery -> IO JQuery
prevAll = jq_prevAll

prevAllSelector :: Selector -> JQuery -> IO JQuery
prevAllSelector s = jq_prevAllSelector (textToJSString s)

prevUntil :: Selector -> Maybe Selector -> JQuery -> IO JQuery
prevUntil s mf = jq_prevUntil (textToJSString s) (maybe "" textToJSString mf)

prevUntilElement :: IsElement e => e -> Maybe Selector -> JQuery -> IO JQuery
prevUntilElement e mf = jq_prevUntil_jq (JQuery . unElement $ toElement e) (maybe "" textToJSString mf)

siblings :: JQuery -> IO JQuery
siblings = jq_siblings

siblingsSelector :: Selector -> JQuery -> IO JQuery
siblingsSelector s = jq_siblingsSelector (textToJSString s)

slice :: Int -> JQuery -> IO JQuery
slice = jq_slice

sliceFromTo :: Int -> Int -> JQuery -> IO JQuery
sliceFromTo = jq_sliceFromTo

stop :: Bool -> JQuery -> IO JQuery
stop = jq_stop
