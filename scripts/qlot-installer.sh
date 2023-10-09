#!/bin/bash

VERSION=${VERSION:-heads/master}

if [ `id -u` == "0" ]; then
  QLOT_BASE=${QLOT_BASE:-/usr/local}
  QLOT_HOME=${QLOT_HOME:-"$QLOT_BASE/lib/qlot"}
  QLOT_SOURCE_DIR="$QLOT_HOME"
  QLOT_LOGS_DIR=/tmp/qlot/logs
  QLOT_BIN_DIR="$QLOT_BASE/bin"
else
  QLOT_HOME=${QLOT_HOME:-~/.qlot}
  QLOT_SOURCE_DIR=${QLOT_SOURCE_DIR:-"$QLOT_HOME/qlot"}
  QLOT_LOGS_DIR="$QLOT_HOME/logs"
  QLOT_BIN_DIR="$QLOT_HOME/bin"
fi

errmsg() { echo -e "\e[31mError: $1\e[0m" >&2; }
notice() { echo -e "\e[33m$1\e[0m"; }
success() { echo -e "\e[32m$1\e[0m"; }

check_requirement() {
  cmd=$1
  if [ "$(which "$cmd" 2>/dev/null)" == "" ]; then
    errmsg "$cmd is required to install Qlot"
    exit 1
  fi
}

if [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros without-roswell=t -L sbcl-bin run --"
else
  errmsg "sbcl is required to setup Qlot."
  exit 1
fi

qlot_version() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval "(asdf:load-asd \"$QLOT_SOURCE_DIR/qlot.asd\")" \
    --eval '(progn (princ (asdf:component-version (asdf:find-system :qlot))) (fresh-line))'
}

check_requirement "curl"
check_requirement "tar"

success 'Welcome to Qlot automatic installer!'
echo ''
echo "Installation path: $QLOT_HOME"

archive="https://github.com/fukamachi/qlot/archive/refs/$VERSION.tar.gz"

if [ -f "$QLOT_SOURCE_DIR/qlot.asd" ]; then
  rm -rf "$QLOT_SOURCE_DIR/"
fi

mkdir -p "$QLOT_HOME"
mkdir -p "$QLOT_SOURCE_DIR"

#
# Download

echo -n "Downloading an archive from '$archive'..."
curl -sL "$archive" | tar zx -C "$QLOT_SOURCE_DIR" --strip-component 1
echo "done"

#
# Setup

cd "$QLOT_SOURCE_DIR"

mkdir -p "$QLOT_LOGS_DIR"
install_log_path="$QLOT_LOGS_DIR/install-$(date '+%s').log"
echo "Setting it up. This may take a while..."
scripts/setup.sh > "$install_log_path" 2>&1

if [ "$?" != "0" ]; then
  errmsg "Setup process is failed. See '$install_log_path' for the detailed logs."
  errmsg "If it can be a bug, please report an issue at https://github.com/fukamachi/qlot/issues."
  exit $?
fi

mkdir -p "$QLOT_BIN_DIR"
printf '#!/bin/sh\nexec %s/scripts/run.sh "$@"\n' "$QLOT_SOURCE_DIR" > "$QLOT_BIN_DIR/qlot"
chmod 755 "$QLOT_BIN_DIR/qlot"

echo ''
success "Qlot v$(qlot_version) has been successfully installed under '$QLOT_HOME'."
echo ''

if [ `id -u` != "0" ]; then
  echo "The executable script is located at '$QLOT_BIN_DIR/qlot'."
  echo "To make it runnable by your shell, please add '$QLOT_BIN_DIR' to '\$PATH'."
  echo ''
  echo "    export PATH=\"$QLOT_BIN_DIR:\$PATH\""
  echo ''
  echo 'Or, copy the script to a searchable directory such as /usr/local/bin.'
  echo ''
  echo "    sudo cp $QLOT_BIN_DIR/qlot /usr/local/bin"
  echo ''
fi
echo 'Enjoy!'
