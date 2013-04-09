{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module JavaScript.JQuery.Internal where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Types.Internal

import Control.Concurrent.MVar

data JQuery_
data Element_
data Event_

type JQuery = JSRef JQuery_
type Element = JSRef Element_
type Event = JSRef Event_

#ifdef __GHCJS__
foreign import javascript unsafe "$2.addClass($1)"       jq_addClass          :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.attr($1)"           jq_getAttr           :: JSString             -> JQuery -> IO JSString
foreign import javascript unsafe "$3.attr($1,$2)"        jq_setAttr           :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.hasClass($1)"       jq_hasClass          :: JSString             -> JQuery -> IO JSBool
foreign import javascript unsafe "$1.html()"             jq_getHtml           ::                         JQuery -> IO JSString
foreign import javascript unsafe "$2.html($1)"           jq_setHtml           :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.prop($1)"           jq_getProp           :: JSString             -> JQuery -> IO JSString
foreign import javascript unsafe "$3.prop($1,$2)"        jq_setProp           :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.removeAttr($1)"     jq_removeAttr        :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.removeClass($1)"    jq_removeClass       :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.removeProp($1)"     jq_removeProp        :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.val()"              jq_getVal            ::                         JQuery -> IO JSString
foreign import javascript unsafe "$2.val($1)"            jq_setVal            :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.text()"             jq_getText           ::                         JQuery -> IO JSString
foreign import javascript unsafe "$2.text($1)"           jq_setText           :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "jQuery.holdReady($1)"  jq_holdReady         :: JSBool                         -> IO ()
foreign import javascript unsafe "jQuery($1)"            jq_selectElement     :: Element                        -> IO JQuery
foreign import javascript unsafe "jQuery($1)"            jq_selectObject      :: JSRef ()                       -> IO JQuery
foreign import javascript unsafe "jQuery($1)"            jq_select            :: JSString                       -> IO JQuery
foreign import javascript unsafe "jQuery()"              jq_selectEmpty       ::                                   IO JQuery
foreign import javascript unsafe "jQuery($1,$2)"         jq_selectWithContext :: JSString -> JSRef ()           -> IO JQuery
foreign import javascript unsafe "$2.css($1)"            jq_getCss            :: JSString             -> JQuery -> IO JSString
foreign import javascript unsafe "$3.css($1,$2)"         jq_setCss            :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.height()"           jq_getHeight         ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.height($1)"         jq_setHeight         :: Double               -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.width()"            jq_getWidth          ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.width($1)"          jq_setWidth          :: Double               -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.innerHeight()"      jq_getInnerHeight    ::                         JQuery -> IO Double
foreign import javascript unsafe "$1.innerWidth()"       jq_getInnerWidth     ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.outerHeight($1)"    jq_getOuterHeight    :: JSBool               -> JQuery -> IO Double
foreign import javascript unsafe "$2.outerWidth($1)"     jq_getOuterWidth     :: JSBool               -> JQuery -> IO Double
foreign import javascript unsafe "$2.trigger($1)"        jq_trigger           :: JSString             -> JQuery -> IO ()
foreign import javascript unsafe "$2.triggerHandler($1)" jq_triggerHandler    :: JSString             -> JQuery -> IO ()
foreign import javascript unsafe "$1.scrollLeft()"       jq_getScrollLeft     ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.scrollLeft($1)"     jq_setScrollLeft     :: Double               -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.scrollTop()"        jq_getScrollTop      ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.scrollTop($1)"      jq_setScrollTop      :: Double               -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.focus()"            jq_focus             ::                         JQuery -> IO JQuery

foreign import javascript unsafe "jQuery($1.delegateTarget)"          jq_delegateTarget                :: Event -> IO Element
foreign import javascript unsafe "$1.isDefaultPrevented()"            jq_isDefaultPrevented            :: Event -> IO JSBool
foreign import javascript unsafe "$1.isImmediatePropagationStopped()" jq_isImmediatePropagationStopped :: Event -> IO JSBool
foreign import javascript unsafe "$1.isPropagationStopped()"          jq_isPropagationStopped          :: Event -> IO JSBool
foreign import javascript unsafe "$1.namespace"                       jq_namespace                     :: Event -> IO JSString
foreign import javascript unsafe "$1.pageX"                           jq_pageX                         :: Event -> IO Double -- fixme might not exist
foreign import javascript unsafe "$1.pageY"                           jq_pageY                         :: Event -> IO Double -- fixme might not exist
foreign import javascript unsafe "$1.preventDefault()"                jq_preventDefault                :: Event -> IO ()
foreign import javascript unsafe "$1.stopPropagation()"               jq_stopPropagation               :: Event -> IO ()
foreign import javascript unsafe "$1.stopImmediatePropagation()"      jq_stopImmediatePropagation      :: Event -> IO ()
foreign import javascript unsafe "$1.target"                          jq_target                        :: Event -> IO Element
foreign import javascript unsafe "$1.timeStamp"                       jq_timeStamp                     :: Event -> IO Double
foreign import javascript unsafe "$1.type"                            jq_eventType                     :: Event -> IO JSString
foreign import javascript unsafe "$1.which"                           jq_eventWhich                    :: Event -> IO Int -- fixme might not exist?

foreign import javascript unsafe "$2.after($1)"                       jq_after             :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.append($1)"                      jq_append            :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.appendTo($1)"                    jq_appendTo          :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.before($1)"                      jq_before            :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$3.clone($1,$2)"                    jq_clone             :: JSBool -> JSBool     -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.detach($1)"                      jq_detach            :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.empty()"                         jq_empty             ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.insertAfter($1)"                 jq_insertAfter       :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.insertBefore($1)"                jq_insertBefore      :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.prepend($1)"                     jq_prepend           :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.prependTo($1)"                   jq_prependTo         :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.remove($1)"                      jq_remove            :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.replaceAll($1)"                  jq_replaceAll        :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.replaceWith($1)"                 jq_replaceWith       :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.unwrap()"                        jq_unwrap            ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.wrap($1)"                        jq_wrap              :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.wrapAll($1)"                     jq_wrapAll           :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.wrapInner($1)"                   jq_wrapInner         :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.add($1)"                         jq_add               :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.andSelf()"                       jq_andSelf           ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.children($1)"                    jq_children          :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.closest($1)"                     jq_closest           :: JSRef ()             -> JQuery -> IO JQuery

foreign import javascript unsafe "$1.contents()"                      jq_contents          ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$1.end()"                           jq_end               ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.eq($1)"                          jq_eq                :: Int                  -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.filter($1)"                      jq_filter            :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.find($1)"                        jq_find              :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.first()"                         jq_first             ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.has($1)"                         jq_has               :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.is($1)"                          jq_is                :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.last()"                          jq_last              ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.next($1)"                        jq_next              :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.nextAll($1)"                     jq_nextAll           :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$3.nextUntil($1,$2)"                jq_nextUntil         :: JSRef () -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.not($1)"                         jq_not               :: JSRef ()             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.offsetParent()"                  jq_offsetParent      ::                         JQuery -> IO JQuery
foreign import javascript unsafe "$2.parent($1)"                      jq_parent            :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.parents($1)"                     jq_parents           :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$3.parentsUntil($1,$2)"             jq_parentsUntil      :: JSRef () -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.prev($1)"                        jq_prev              :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.prevAll($1)"                     jq_prevAll           :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$3.prevUntil($1,$2)"                jq_prevUntil         :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.siblings($1)"                    jq_siblings          :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.slice($1)"                       jq_slice             :: Int                  -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.slice($1,$2)"                    jq_sliceFromTo       :: Int -> Int           -> JQuery -> IO JQuery


foreign import javascript unsafe "$8.on($2, $3, $4, h$makeMVarListener($1, $5, $6, $7))"
  jq_on :: JSObject (MVar Event)
        -> JSString               -- ^ event type
        -> JSString               -- ^ descendant selector
        -> JSRef a                -- ^ data
        -> JSBool                 -- ^ stopPropagation
        -> JSBool                 -- ^ stopImmediatePropagation
        -> JSBool                 -- ^ preventDefault
        -> JQuery
        -> IO JQuery

foreign import javascript unsafe "$8.one($2, $3, $4, h$makeMVarListener($1, $5, $6, $7))"
  jq_one :: JSObject (MVar Event)
         -> JSString               -- ^ event type
         -> JSString               -- ^ descendant selector
         -> JSRef a                -- ^ data
         -> JSBool                 -- ^ stopPropagation
         -> JSBool                 -- ^ stopImmediatePropagation
         -> JSBool                 -- ^ preventDefault
         -> JQuery
         -> IO JQuery

#else
jq_on                            = error "jq_on: only available in JavaScript"
jq_one                           = error "jq_one: only available in JavaScript"

#include "nonghcjs.txt"

#endif
