#!/bin/bash

# docker build -t docker-qlot-test tests
# docker run --rm -it -v $PWD:/app docker-qlot-test tests/run-tests.sh

cat scripts/qlot-installer.sh | bash

[ -d ~/.qlot ] && [ -f /usr/local/bin/qlot ] || exit 1
