#!/bin/sh

set -eu

VERSION=${VERSION:-heads/master}
QLOT_ARCHIVE=${QLOT_ARCHIVE:-https://github.com/fukamachi/qlot/archive/refs/$VERSION.tar.gz}

if [ "$(id -u)" -eq 0 ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_HOME=${QLOT_HOME:-"$QLOT_BASE/lib/qlot"}
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-"$QLOT_BASE/bin"}
  QLOT_TMP_DIR=/tmp/qlot
  QLOT_SOURCE_DIR="$QLOT_HOME"
  QLOT_LOGS_DIR=/tmp/qlot/logs
else
  if [ -n "${XDG_DATA_HOME:-}" ]; then
    QLOT_HOME="$XDG_DATA_HOME/qlot"
  else
    QLOT_HOME=${QLOT_HOME:-~/.qlot}
  fi
  QLOT_BIN_DIR=${QLOT_BIN_DIR:-${XDG_BIN_HOME:-"$QLOT_HOME/bin"}}
  QLOT_TMP_DIR="$QLOT_HOME/tmp"
  QLOT_SOURCE_DIR=${QLOT_SOURCE_DIR:-"$QLOT_HOME/qlot"}
  QLOT_LOGS_DIR="$QLOT_HOME/logs"
fi

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }
errmsg() { printf "%sError: %s%s\n" "$(ansi 31 1)" "$1" "$(ansi 0)"; }
notice() { printf "%s%s%s\n" "$(ansi 33)" "$1" "$(ansi 0)"; }
success() { printf "%s%s%s\n" "$(ansi 32)" "$1" "$(ansi 0)"; }

check_requirement() {
  for cmd in "$@"
  do
    if [ "$(which "$cmd" 2>/dev/null)" != "" ]; then
      return
    fi
  done
  if [ $# -eq 1 ]; then
    errmsg "$1 is required to install Qlot"
  else
    all_cmds=$(printf '%s, ' "$@")
    errmsg "One of ${all_cmds%, } is required to install Qlot"
  fi
  exit 1
}

check_requirement "which"

if [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros +Q -L sbcl-bin run --"
else
  errmsg "sbcl is required to setup Qlot."
  exit 1
fi

# Check if a directory is in PATH
check_in_path() {
  case ":$PATH:" in
    *":$1:"*) return 0 ;;
    *) return 1 ;;
  esac
}

qlot_version() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval "(asdf:load-asd \"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(progn (princ (asdf:component-version (asdf:find-system :qlot))) (fresh-line))'
}

check_requirement "curl" "wget"
check_requirement "tar"

success 'Welcome to Qlot automatic installer!'
echo ''
echo "Installation path: $QLOT_HOME"

mkdir -p "$QLOT_HOME"
mkdir -p "$QLOT_TMP_DIR"

#
# Download

if [ -f "$QLOT_TMP_DIR/qlot.tar.gz" ]; then
  echo "Already have an archive: $QLOT_TMP_DIR/qlot.tar.gz"
else
  printf "Downloading an archive from '%s'..." "$QLOT_ARCHIVE"
  if [ "$(which curl 2>/dev/null)" != "" ]; then
    curl -sL "$QLOT_ARCHIVE" -o "$QLOT_TMP_DIR/qlot.tar.gz"
  else
    wget -q "$QLOT_ARCHIVE" -O "$QLOT_TMP_DIR/qlot.tar.gz"
  fi
fi

tar zxf "$QLOT_TMP_DIR/qlot.tar.gz" -C "$QLOT_TMP_DIR"

if [ -d "$QLOT_SOURCE_DIR/" ]; then
  rm -rf "${QLOT_SOURCE_DIR:?}/"
fi
mv "$(find "$QLOT_TMP_DIR" -maxdepth 1 -mindepth 1 -type d -name "qlot*")" "$QLOT_SOURCE_DIR"

echo "done"

#
# Setup

cd "$QLOT_SOURCE_DIR"

mkdir -p "$QLOT_LOGS_DIR"
install_log_path="$QLOT_LOGS_DIR/install-$(date '+%s').log"
echo "Setting it up. This may take a while..."
setup_success=$(scripts/setup.sh > "$install_log_path" 2>&1)

if [ "$setup_success" -ne 0 ]; then
  errmsg "Setup process is failed. See '$install_log_path' for the detailed logs."
  errmsg "If it can be a bug, please report an issue at https://github.com/fukamachi/qlot/issues."
  exit "$setup_success"
fi

install_success=$(QLOT_BIN_DIR="$QLOT_BIN_DIR" scripts/install.sh >> "$install_log_path" 2>&1)

if [ "$install_success" -ne 0 ]; then
  errmsg "Install process is failed. See '$install_log_path' for the detailed logs."
  errmsg "If it can be a bug, please report an issue at https://github.com/fukamachi/qlot/issues."
  exit "$install_success"
fi

echo ''
success "Qlot v$(qlot_version) has been successfully installed under '$QLOT_HOME'."
echo ''

if [ "$(id -u)" -ne 0 ]; then
  if ! check_in_path "$QLOT_BIN_DIR"; then
    echo "$(ansi 33)The executable script is located at '$QLOT_BIN_DIR/qlot'.$(ansi 0)"
    echo "To make it runnable by your shell, please add '$QLOT_BIN_DIR' to '\$PATH'."
    echo ''
    echo "    export PATH=\"$QLOT_BIN_DIR:\$PATH\""
    echo ''
  fi
fi
echo 'Enjoy!'
