#!/bin/sh

set -eux

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 >/dev/null && pwd -P)

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }

if [ `id -u` -eq 0 ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-"$QLOT_BASE/bin"}
else
  QLOT_HOME=${QLOT_HOME:-~/.qlot}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-"$QLOT_HOME/bin"}
fi

if [ ! -d "$QLOT_BIN_DIR" ]; then
  errmsg "Directory not exist: $QLOT_BIN_DIR"
  exit 1
fi

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
  XDG_DATA_HOME=/usr/local/share
else
  XDG_DATA_HOME=${XDG_DATA_HOME:-~/.local/share}
fi
REGISTRY_DIR="$XDG_DATA_HOME/common-lisp/systems"

mkdir -p "$REGISTRY_DIR"
if [ ! -f "$REGISTRY_DIR/qlot.asd" ]; then
  ln -s "$QLOT_SOURCE_DIR/qlot.asd" "$REGISTRY_DIR/qlot.asd"
fi
