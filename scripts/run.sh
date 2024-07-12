#!/bin/sh

set -e

QLOT_SOURCE_DIR=$(dirname -- "${0%/*}")

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
if ! [ -t 1 ]; then
  ansi() { :; }
  export QLOT_NO_TERMINAL=1
fi
errmsg() { printf "%sError: %s%s\n" "$(ansi 31)" "$1" "$(ansi 0)"; }

# shellcheck disable=SC2153
if [ "$LISP" = "" ]; then
  if [ "$(which ros 2>/dev/null)" != "" ]; then
    lisp="ros"
  elif [ "$(which sbcl 2>/dev/null)" != "" ]; then
    lisp="sbcl"
  elif [ "$(which ecl 2>/dev/null)" != "" ]; then
    lisp="ecl"
  elif [ "$(which clasp 2>/dev/null)" != "" ]; then
    lisp="clasp"
  elif [ "$(which abcl 2>/dev/null)" != "" ]; then
    lisp="abcl"
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
  abcl)
    lisp_cmd="abcl"
    lisp_init_options="--noinform --noinit --nosystem"
    eval_option="--eval"
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

# shellcheck disable=SC2086
exec $lisp_cmd $lisp_init_options \
  "$eval_option" "(let (*load-verbose*) (load \"$QLOT_SETUP_FILE\"))" \
  "$eval_option" "(let (*load-verbose*) (asdf:load-asd #P\"$QLOT_SOURCE_DIR/qlot.asd\"))" \
  "$eval_option" '(let ((*standard-output* (make-broadcast-stream)) (*trace-output* (make-broadcast-stream))) (asdf:load-system :qlot/cli))' \
  "$eval_option" '(qlot/cli:main)' -- "$@"
