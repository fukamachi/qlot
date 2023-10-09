#!/bin/bash

QLOT_HOME=${QLOT_HOME:-~/.qlot}

rm -rf "$QLOT_HOME"

if [ `id -u` == "0" ]; then
  rm /usr/local/bin/qlot
fi

if [ "$(which sbcl 2>/dev/null)" != "" ]; then
  lisp="sbcl"
elif [ "$(which ros 2>/dev/null)" != "" ]; then
  lisp="ros without-roswell=t -L sbcl-bin run --"
else
  exit
fi

systems_directory() {
  $lisp --noinform --no-sysinit --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval '(princ (uiop:native-namestring (uiop:xdg-data-home #P"common-lisp/systems/")))'
}

registry_dir=$(systems_directory)
rm -f "${registry_dir}qlot.asd"

echo -e "\e[32mQlot has been successfully deleted.\e[0m"
echo 'Bye!'
