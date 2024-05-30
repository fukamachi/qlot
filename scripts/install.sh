#!/bin/sh

set -eux

QLOT_SOURCE_DIR=$(realpath "$(dirname -- "${0%/*}")")

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }

if [ `id -u` -eq 0 ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-$QLOT_BASE/bin}
else
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-${XDG_BIN_HOME:-~/.qlot/bin}}
fi

mkdir -p "$QLOT_BIN_DIR"

if [ -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
  SETUP_FILE="$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp"
elif [ -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
  SETUP_FILE="$QLOT_SOURCE_DIR/.qlot/setup.lisp"
else
  errmsg "Qlot isn't setup yet. Run 'scripts/setup.sh' first."
  exit 1
fi

printf '#!/bin/sh\nexport QLOT_SETUP_FILE=%s\nexec %s/scripts/run.sh "$@"\n' \
  "$SETUP_FILE" "$QLOT_SOURCE_DIR" > "$QLOT_BIN_DIR/qlot"
chmod 755 "$QLOT_BIN_DIR/qlot"

if [ `id -u` -eq 0 ]; then
  REGISTRY_DIR=/usr/local/share/common-lisp/systems
else
  REGISTRY_DIR="${XDG_DATA_HOME:-~/.local/share}/common-lisp/systems"
fi

mkdir -p "$REGISTRY_DIR"
if [ ! -f "$REGISTRY_DIR/qlot.asd" ]; then
  ln -s "$QLOT_SOURCE_DIR/qlot.asd" "$REGISTRY_DIR/qlot.asd"
fi
