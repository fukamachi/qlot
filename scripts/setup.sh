#!/bin/bash

set -eux

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 >/dev/null && pwd -P)

errmsg() { echo -e "\e[31mError: $1\e[0m" >&2; }
if [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros without-roswell=t -L sbcl-bin run --"
else
  errmsg "sbcl is required to setup Qlot."
  exit 1
fi

export QLOT_FETCH=curl

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

systems_directory() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval '(princ (uiop:native-namestring (uiop:xdg-data-home #P"common-lisp/systems/")))'
}
registry_dir=$(systems_directory)
mkdir -p "$registry_dir"
if [ ! -f "${registry_dir}qlot.asd" ]; then
  ln -s "$QLOT_SOURCE_DIR/qlot.asd" "${registry_dir}qlot.asd"
fi
