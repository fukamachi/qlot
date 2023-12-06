#!/bin/sh

QLOT_HOME=${QLOT_HOME:-~/.qlot}

ansi() {
  [ $# -gt 0 ] || return
  printf "\033[%sm" "$@"
}
[ -t 1 ] || ansi() { :; }

rm -rf "$QLOT_HOME"

if [ `id -u` == "0" ]; then
  rm /usr/local/bin/qlot
fi

if [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros +Q -L sbcl-bin run --"
else
  exit
fi

systems_directory() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval '(princ (uiop:native-namestring (uiop:xdg-data-home #P"common-lisp/systems/")))'
}

registry_dir=$(systems_directory)
rm -f "${registry_dir}qlot.asd"

printf "%sQlot has been successfully deleted.%s\n" "$(ansi 32)" "$(ansi 0)"
echo 'Bye!'
