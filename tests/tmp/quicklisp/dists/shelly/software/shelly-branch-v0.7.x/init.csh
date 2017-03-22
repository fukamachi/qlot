#!/bin/sh
if ( ${?SHELLY_HOME} ) then
    setenv SHELLY_HOME "$SHELLY_HOME"
else
    setenv SHELLY_HOME "$HOME/.shelly"
endif
setenv PATH "$SHELLY_HOME/bin:$PATH"
