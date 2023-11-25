#!/bin/bash

set -eu

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 && pwd -P)

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" $1 "$(ansi 0)"; }

if [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros +Q -L sbcl-bin run --"
elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
else
  errmsg "sbcl is required by Qlot."
  exit 1
fi

if [ -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
  setup_file="$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp"
elif [ -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
  setup_file="$QLOT_SOURCE_DIR/.qlot/setup.lisp"
else
  echo "Qlot is not setup yet." >&2
  echo "Run '$QLOT_SOURCE_DIR/scripts/setup.sh' first." >&2
  exit 1
fi

exec $lisp --noinform --no-sysinit --no-userinit --non-interactive \
  --load "$setup_file" \
  --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  --eval '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (asdf:load-system :qlot/fetch))' \
  --eval '(qlot/fetch::main)' -- "$@"
