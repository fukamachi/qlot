#!/bin/sh

set -eu

if [ "$(id -u)" -eq 0 ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_HOME=${QLOT_HOME:-"$QLOT_BASE/lib/qlot"}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-"$QLOT_BASE/bin"}
else
  if [ -n "${XDG_DATA_HOME:-}" ]; then
    QLOT_HOME="$XDG_DATA_HOME/qlot"
  else
    QLOT_HOME=${QLOT_HOME:-~/.qlot}
  fi
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-${XDG_BIN_HOME:-"$QLOT_HOME/bin"}}
fi

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }

rm "$QLOT_BIN_DIR"/qlot
rm -r "$QLOT_HOME"

if [ "$(id -u)" -eq 0 ]; then
  REGISTRY_DIR=/usr/local/share/common-lisp/systems
else
  REGISTRY_DIR="${XDG_DATA_HOME:-~/.local/share}/common-lisp/systems"
fi

if [ -d "$REGISTRY_DIR" ]; then
  rm -f "$REGISTRY_DIR/qlot.asd"
fi

printf "%sQlot has been successfully deleted.%s\n" "$(ansi 32)" "$(ansi 0)"
echo 'Bye!'
