#!/bin/sh

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

if [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros +Q -L sbcl-bin run --"
elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
else
  errmsg "sbcl is required to run Qlot."
  exit 1
fi

if [ "$QLOT_SETUP_FILE" = "" ]; then
  if [ -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
    QLOT_SETUP_FILE="$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp"
  elif [ -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
    QLOT_SETUP_FILE="$QLOT_SOURCE_DIR/.qlot/setup.lisp"
  else
    echo "Qlot is not setup yet." >&2
    echo "Run '$QLOT_SOURCE_DIR/scripts/setup.sh' first." >&2
    exit 1
  fi
fi

exec $lisp --noinform --no-sysinit --no-userinit --non-interactive \
  --load "$QLOT_SETUP_FILE" \
  --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  --eval '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (asdf:load-system :qlot/cli))' \
  --eval '(qlot/cli:main)' -- "$@"
