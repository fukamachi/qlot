#!/bin/bash

set -eux

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" && pwd -P)

if [ ! -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
  sbcl --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' \
    --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(asdf:load-system :qlot/install/quicklisp)' \
    --eval "(qlot/install/quicklisp:install-quicklisp \"$QLOT_SOURCE_DIR/.qlot/\")"
else
  sbcl --noinform --no-sysinit --no-userinit --non-interactive \
    --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
    --eval '(ql:update-all-dists :prompt nil)'
fi

sbcl --noinform --no-sysinit --no-userinit --non-interactive \
  --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
  --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  --eval '(ql:quickload (list :qlot :qlot/distify))'
