#!/bin/bash

set -eux

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 >/dev/null && pwd -P)

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
    --eval '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (mapc (function asdf:load-system) (list :qlot :qlot/cli :qlot/distify)))'
else
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --load "$QLOT_SOURCE_DIR/.qlot/setup.lisp" \
    --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(ql:quickload (list :qlot :qlot/cli :qlot/distify))'
fi

systems_directory() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval '(princ (uiop:native-namestring (uiop:xdg-data-home #P"common-lisp/systems/")))'
}
registry_dir=$(systems_directory)
mkdir -p "$registry_dir"
if [ ! -f "${registry_dir}qlot.asd" ]; then
  ln -s "$QLOT_SOURCE_DIR/qlot.asd" "${registry_dir}qlot.asd"
fi
