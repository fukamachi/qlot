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

  if [ -z "${QLOT_BIN_DIR:-}" ]; then
    if [ -n "${XDG_BIN_HOME:-}" ]; then
      QLOT_BIN_DIR="$XDG_BIN_HOME"
    elif check_in_path "$HOME/.local/bin"; then
      QLOT_BIN_DIR="$HOME/.local/bin"
    else
      QLOT_BIN_DIR="$QLOT_HOME/bin"
    fi
  fi
fi

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }

rm -f "$QLOT_BIN_DIR"/qlot
rm -f "$QLOT_HOME/bin/qlot"  # backward compatibility

# If QLOT_HOME is a symlink, just remove the link
# Otherwise, recursively delete the directory
if [ -L "$QLOT_HOME" ]; then
  rm "$QLOT_HOME"
elif [ -d "$QLOT_HOME" ]; then
  rm -r "$QLOT_HOME"
fi

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
