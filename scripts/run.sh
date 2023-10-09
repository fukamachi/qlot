#!/bin/bash

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 && pwd -P)
command=$1

check_qlot_directory() {
  if [ ! -f .qlot/setup.lisp ]; then
    echo ".qlot/ is not a quicklisp directory." >&2
    echo "Run 'qlot install' first." >&2
    exit 1
  fi
}
errmsg() { echo -e "\e[31mError: $1\e[0m" >&2; }

if [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros without-roswell=t -L sbcl-bin run --"
elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
else
  errmsg "sbcl is required to run Qlot."
  exit 1
fi

case "$command" in
  run)
    check_qlot_directory
    shift
    exec $lisp --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp "$@"
    ;;
  *)
    exec $lisp --noinform --no-sysinit --no-userinit --non-interactive \
      --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
      --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
      --eval '(ql:quickload :qlot/cli :silent t)' \
      --eval '(qlot/cli:main)' -- "$@"
esac
