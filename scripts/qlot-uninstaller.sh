#!/bin/sh

set -eux

if [ `id -u` -eq 0 ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_HOME=${QLOT_HOME:-"$QLOT_BASE/lib/qlot"}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-"$QLOT_BASE/bin"}
else
  QLOT_HOME=${QLOT_HOME:-~/.qlot}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-"$QLOT_HOME/bin"}
fi

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }

rm "$QLOT_BIN_DIR"/qlot
rm -r "$QLOT_HOME"

if [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros +Q -L sbcl-bin run --"
else
  exit 1
fi

systems_directory() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval '(princ (uiop:native-namestring (uiop:xdg-data-home #P"common-lisp/systems/")))'
}

registry_dir=$(systems_directory)
if [ -d "$registry_dir" ]; then
  rm -f "${registry_dir}qlot.asd"
fi

printf "%sQlot has been successfully deleted.%s\n" "$(ansi 32)" "$(ansi 0)"
echo 'Bye!'
