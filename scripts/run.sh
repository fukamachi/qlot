#!/bin/bash

set -eu

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 && pwd -P)

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
if ! [ -t 1 ]; then
  ansi() { :; }
  export QLOT_NO_COLOR=1
fi
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }

if [ -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
  setup_file="$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp"
elif [ -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
  setup_file="$QLOT_SOURCE_DIR/.qlot/setup.lisp"
fi

if [ "$(which ros 2>/dev/null)" != "" ]; then
  if [ "$setup_file" != "" ]; then
    lisp="ros +Q -L sbcl-bin run -- --noinform --no-sysinit --no-userinit --non-interactive --load $setup_file"
  else
    lisp="ros -L sbcl-bin run -- --noinform --non-interactive"
  fi
elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
  if [ "$setup_file" != "" ]; then
    lisp="sbcl --noinform --no-sysinit --no-userinit --non-interactive --load $setup_file"
  else
    lisp="sbcl --noinform --non-interactive"
  fi
else
  errmsg "sbcl is required to run Qlot."
  exit 1
fi

exec $lisp \
  --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  --eval '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (asdf:load-system :qlot/cli))' \
  --eval '(qlot/cli:main)' -- "$@"
