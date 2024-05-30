#!/bin/sh

set -eux

QLOT_SOURCE_DIR=$(realpath "$(dirname -- "${0%/*}")")

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }
if [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros +Q -L sbcl-bin run --"
elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
else
  errmsg "sbcl is required to setup Qlot."
  exit 1
fi

export QLOT_FETCH=curl

if [ ! -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
  if [ ! -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
    $lisp --noinform --no-sysinit --no-userinit --non-interactive \
      --eval '(require :asdf)' \
      --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
      --eval '(asdf:load-system :qlot/install/quicklisp)' \
      --eval "(qlot/install/quicklisp:install-quicklisp \"$QLOT_SOURCE_DIR/.qlot/\")"
    if [ ! -d "$QLOT_SOURCE_DIR/.qlot/dists/quicklisp" ]; then
      $lisp --noinform --no-sysinit --no-userinit --non-interactive \
        --load "$QLOT_SOURCE_DIR/.qlot/setup.lisp" \
        --eval '(ql-dist:install-dist "https://beta.quicklisp.org/dist/quicklisp.txt" :prompt nil)'
    fi
  fi
fi

if [ -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --load "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" \
    --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (mapc (function asdf:load-system) (list :qlot :qlot/subcommands :qlot/cli :qlot/fetch)))'
else
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --load "$QLOT_SOURCE_DIR/.qlot/setup.lisp" \
    --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(ql:quickload (list :qlot :qlot/subcommands :qlot/cli :qlot/fetch))'
fi
