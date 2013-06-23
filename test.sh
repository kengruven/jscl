#!/bin/sh

# Mac OS X: jsc
# Linux: ??
# Windows: can "CSCRIPT" be used for this?

/System/Library/Frameworks/JavaScriptCore.framework/Versions/Current/Resources/jsc jscl.js -e "lisp.write = function(str) { print(xstring(str).trim()); }" tests.js
