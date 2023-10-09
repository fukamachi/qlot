#!/bin/bash

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 && pwd -P)

errmsg() { echo -e "\e[31mError: $1\e[0m" >&2; }
if [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros without-roswell=t -L sbcl-bin run --"
elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
else
  errmsg "sbcl is required by Qlot."
  exit 1
fi

exec $lisp --noinform --no-sysinit --no-userinit --non-interactive \
  --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
  --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  --eval '(ql:quickload :qlot/fetch :silent t)' \
  --eval '(qlot/fetch::main)' -- "$@"
