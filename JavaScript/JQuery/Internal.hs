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
foreign import javascript unsafe "$2.addClass($1)"    jq_addClass          :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.attr($1)"        jq_getAttr           :: JSString             -> JQuery -> IO JSString
foreign import javascript unsafe "$3.attr($1,$2)"     jq_setAttr           :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.hasClass($1)"    jq_hasClass          :: JSString             -> JQuery -> IO JSBool
foreign import javascript unsafe "$1.html()"          jq_getHtml           ::                         JQuery -> IO JSString
foreign import javascript unsafe "$2.html($1)"        jq_setHtml           :: JSObject a           -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.prop($1)"        jq_getProp           :: JSString             -> JQuery -> IO JSString
foreign import javascript unsafe "$3.prop($1,$2)"     jq_setProp           :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.removeAttr($1)"  jq_removeAttr        :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.removeClass($1)" jq_removeClass       :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$2.removeProp($1)"  jq_removeProp        :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.val()"           jq_getVal            ::                         JQuery -> IO JSString
foreign import javascript unsafe "$2.val($1)"         jq_setVal            :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.text()"          jq_getText           ::                         JQuery -> IO JSString
foreign import javascript unsafe "$2.text($1)"        jq_setText           :: JSString             -> JQuery -> IO JQuery
foreign import javascript unsafe "jQuery.holdReady($1)" jq_holdReady       :: JSBool                         -> IO ()
foreign import javascript unsafe "jQuery($1)"         jq_selectElement     :: Element                        -> IO JQuery
foreign import javascript unsafe "jQuery($1)"         jq_selectObject      :: JSObject a                     -> IO JQuery
foreign import javascript unsafe "jQuery($1)"         jq_select            :: JSString                       -> IO JQuery
foreign import javascript unsafe "jQuery()"           jq_selectEmpty       ::                                   IO JQuery
foreign import javascript unsafe "jQuery($1,$2)"      jq_selectWithContext :: JSString -> JSObject a         -> IO JQuery
foreign import javascript unsafe "$2.css($1)"         jq_getCss            :: JSString             -> JQuery -> IO JSString
foreign import javascript unsafe "$3.css($1,$2)"      jq_setCss            :: JSString -> JSString -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.height()"        jq_getHeight         ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.height($1)"      jq_setHeight         :: Double               -> JQuery -> IO JQuery
foreign import javascript unsafe "$1.innerHeight()"   jq_getInnerHeight    ::                         JQuery -> IO Double
foreign import javascript unsafe "$1.innerWidth()"    jq_getInnerWidth     ::                         JQuery -> IO Double
foreign import javascript unsafe "$2.outerHeight($1)" jq_getOuterHeight    :: JSBool               -> JQuery -> IO Double
foreign import javascript unsafe "$2.outerWidth($1)"  jq_getOuterWidth     :: JSBool               -> JQuery -> IO Double
foreign import javascript unsafe "$3.bind($1,h$makeMVarListener($2))" jq_bind :: JSString -> JSObject (MVar Event) -> JQuery -> IO JQuery
#else
jq_addClass          = error "jq_addClass: only available in JavaScript"
jq_getAttr           = error "jq_getAttr: only available in JavaScript"
jq_setAttr           = error "jq_setAttr: only available in JavaScript"
jq_hasClass          = error "jq_hasClass: only available in JavaScript"
jq_getHtml           = error "jq_getHtml: only available in JavaScript"
jq_setHtml           = error "jq_setHtml: only available in JavaScript"
jq_getProp           = error "jq_getProp: only available in JavaScript"
jq_setProp           = error "jq_setProp: only available in JavaScript"
jq_removeAttr        = error "jq_removeAttr: only available in JavaScript"
jq_removeClass       = error "jq_removeClass: only available in JavaScript"
jq_removeProp        = error "jq_removeProp: only available in JavaScript"
jq_getVal            = error "jq_getVal: only available in JavaScript"
jq_setVal            = error "jq_setVal: only available in JavaScript"
jq_getText           = error "jq_getText: only available in JavaScript"
jq_setText           = error "jq_setText: only available in JavaScript"
jq_holdReady         = error "jq_holdReady: only available in JavaScript"
jq_selectElement     = error "jq_selectElement: only available in JavaScript"
jq_selectObject      = error "jq_selectObject: only available in JavaScript"
jq_select            = error "jq_select: only available in JavaScript"
jq_selectWithContext = error "jq_selectWithContext: only available in JavaScript"
jq_selectEmpty       = error "jq_selectEmpty: only available in JavaScript"
jq_getCss            = error "js_getCss: only available in JavaScript"
jq_setCss            = error "jq_setCss: only available in JavaScript"
jq_getInnerWidth     = error "jq_getInnerWidth: only available in JavaScript"
jq_getOuterWidth     = error "jq_getOuterWidth: only available in JavaScript"
jq_getInnerHeight    = error "jq_getInnerHeight: only available in JavaScript"
jq_getOuterHeight    = error "jq_getOuterHeight: only available in JavaScript"
jq_getHeight         = error "jq_getHeight: only available in JavaScript"
jq_setHeight         = error "jq_setHeight: only available in JavaScript"
jq_bind              = error "jq_bind: only available in JavaScript"
#endif
