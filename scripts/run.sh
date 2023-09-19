#!/bin/bash

QLOT_HOME=$(cd "$(dirname "$0")/../" && pwd -P)

sbcl --noinform --no-sysinit --no-userinit --non-interactive \
  --load $QLOT_HOME/.qlot/setup.lisp \
  --eval "(asdf:load-asd #P\"$QLOT_HOME/qlot.asd\")" \
  --eval '(ql:quickload :qlot/cli :silent t)' \
  --eval '(qlot/cli:main)' "$@"
