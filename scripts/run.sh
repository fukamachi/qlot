#!/bin/sh

set -e

QLOT_SOURCE_DIR=$(cd "$(dirname "$0")/../" 2>&1 && pwd -P)

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
if ! [ -t 1 ]; then
  ansi() { :; }
  export QLOT_NO_TERMINAL=1
fi
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }

if [ "$LISP" = "" ]; then
  if [ "$(which ros 2>/dev/null)" != "" ]; then
    lisp="ros"
  elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
    lisp="sbcl"
  elif [ "$(which ecl 2>/dev/null)" != "" ]; then
    lisp="ecl"
  elif [ "$(which clasp 2>/dev/null)" != "" ]; then
    lisp="clasp"
  else
    errmsg "A Lisp implementation is required to run Qlot."
    exit 1
  fi
else
  lisp="$LISP"
fi

case "$lisp" in
  ros)
    lisp_cmd="ros +Q -L sbcl-bin run --"
    lisp_init_options="--noinform --no-sysinit --no-userinit --non-interactive"
    eval_option="--eval"
    ;;
  sbcl)
    lisp_cmd="sbcl"
    lisp_init_options="--noinform --no-sysinit --no-userinit --non-interactive"
    eval_option="--eval"
    ;;
  ecl)
    lisp_cmd="ecl"
    lisp_init_options="-norc"
    eval_option="-eval"
    ;;
  clasp)
    lisp_cmd="clasp"
    lisp_init_options="--noinform --norc --non-interactive"
    eval_option="-e"
    ;;
  clisp)
    lisp_cmd="clisp"
    lisp_init_options="-norc --quiet --silent -on-error exit"
    eval_option="-x"
    ;;
  *)
    lisp_cmd="$lisp"
    ;;
esac

if [ "$QLOT_SETUP_FILE" = "" ]; then
  if [ -f "$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp" ]; then
    QLOT_SETUP_FILE="$QLOT_SOURCE_DIR/.bundle-libs/bundle.lisp"
  elif [ -f "$QLOT_SOURCE_DIR/.qlot/setup.lisp" ]; then
    QLOT_SETUP_FILE="$QLOT_SOURCE_DIR/.qlot/setup.lisp"
  else
    echo "Qlot is not setup yet." >&2
    echo "Run '$QLOT_SOURCE_DIR/scripts/setup.sh' first." >&2
    exit 1
  fi
fi

exec $lisp_cmd $lisp_init_options \
  "$eval_option" "(load \"$QLOT_SETUP_FILE\")" \
  "$eval_option" "(asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\")" \
  "$eval_option" '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (asdf:load-system :qlot/cli))' \
  "$eval_option" '(qlot/cli:main)' -- "$@"
