function h$jquery_makeListener(callback, stopProp, stopImmProp, preventDefault) {
    return function(e) {
        if(stopProp) e.stopPropagation();
        if(stopImmProp) e.stopImmediatePropagation();
        if(preventDefault) e.preventDefault();
        callback(e);
    }
}
