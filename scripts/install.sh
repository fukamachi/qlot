#!/bin/sh

set -eux

QLOT_SOURCE_DIR=$(realpath "$(dirname -- "${0%/*}")")

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }

# Check if a directory is in PATH
check_in_path() {
  case ":$PATH:" in
    *":$1:"*) return 0 ;;
    *) return 1 ;;
  esac
}

if [ "$(id -u)" -eq 0 ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-$QLOT_BASE/bin}
else
  # Priority: QLOT_BIN_DIR > XDG_BIN_HOME > ~/.local/bin (if in PATH) > ~/.qlot/bin
  if [ -z "${QLOT_BIN_DIR:-}" ]; then
    if [ -n "${XDG_BIN_HOME:-}" ]; then
      QLOT_BIN_DIR="$XDG_BIN_HOME"
    elif check_in_path "$HOME/.local/bin"; then
      QLOT_BIN_DIR="$HOME/.local/bin"
    else
      QLOT_BIN_DIR="$HOME/.qlot/bin"
    fi
  fi
fi

mkdir -p "$QLOT_BIN_DIR"

# Check that Qlot is setup
if [ ! -f "$QLOT_SOURCE_DIR/.bundle-libs/setup.lisp" ] && \
   [ ! -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ] && \
   [ ! -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
  errmsg "Qlot isn't setup yet. Run 'scripts/setup.sh' first."
  exit 1
fi

ln -sf "$QLOT_SOURCE_DIR/bin/qlot" "$QLOT_BIN_DIR/qlot"

if [ "$(id -u)" -eq 0 ]; then
  REGISTRY_DIR=/usr/local/share/common-lisp/systems
else
  REGISTRY_DIR="${XDG_DATA_HOME:-~/.local/share}/common-lisp/systems"
fi

mkdir -p "$REGISTRY_DIR"
if [ ! -f "$REGISTRY_DIR/qlot.asd" ]; then
  ln -sf "$QLOT_SOURCE_DIR/qlot.asd" "$REGISTRY_DIR/qlot.asd"
fi
