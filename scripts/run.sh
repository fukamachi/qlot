#!/bin/bash

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" && pwd -P)
command=$1

check_qlot_directory() {
  if [ ! -f .qlot/setup.lisp ]; then
    echo ".qlot/ is not a quicklisp directory." >&2
    echo "Run 'qlot install' first." >&2
    exit 1
  fi
}

case "$command" in
  run)
    check_qlot_directory
    shift
    exec sbcl --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp "$@"
    ;;
  *)
    sbcl --noinform --no-sysinit --no-userinit --non-interactive \
      --load $QLOT_SOURCE_DIR/.qlot/setup.lisp \
      --eval "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
      --eval '(ql:quickload :qlot/cli :silent t)' \
      --eval '(qlot/cli:main)' "$@"
esac
