#!/bin/bash

set -eux

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" && pwd -P)

errmsg() { echo -e "\e[31mError: $1\e[0m" >&2; }
if [ "$(which sbcl)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros)" != "" ]; then
  lisp="ros without-roswell=t -L sbcl-bin run --"
else
  errmsg "sbcl is required to setup Qlot."
  exit 1
fi

if [ ! -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' \
    --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(asdf:load-system :qlot/install/quicklisp)' \
    --eval "(qlot/install/quicklisp:install-quicklisp \"$QLOT_SOURCE_DIR/.qlot/\")"
else
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
    --eval '(ql:update-all-dists :prompt nil)'
fi

$lisp --noinform --no-sysinit --no-userinit --non-interactive \
  --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
  --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  --eval '(ql:quickload (list :qlot :qlot/distify))'
