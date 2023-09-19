#!/bin/bash

set -eux

QLOT_HOME=$(cd "$(dirname "$0")/../" && pwd -P)

if [ ! -f "$QLOT_HOME/.qlot/setup.lisp" ]; then
  sbcl --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' \
    --eval "(asdf:load-asd #P\"$QLOT_HOME/qlot.asd\")" \
    --eval '(asdf:load-system :qlot/install/quicklisp)' \
    --eval "(qlot/install/quicklisp:install-quicklisp \"$QLOT_HOME/.qlot/\")"
else
  sbcl --noinform --no-sysinit --no-userinit --non-interactive \
    --load $QLOT_HOME/.qlot/setup.lisp \
    --eval '(ql:update-all-dists :prompt nil)'
fi

sbcl --noinform --no-sysinit --no-userinit --non-interactive \
  --load $QLOT_HOME/.qlot/setup.lisp \
  --eval "(asdf:load-asd #P\"$QLOT_HOME/qlot.asd\")" \
  --eval '(ql:quickload (list :qlot :qlot/distify))'
