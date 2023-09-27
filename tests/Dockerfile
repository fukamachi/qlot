FROM fukamachi/sbcl

WORKDIR /app

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends git curl && \
  rm -rf /var/lib/apt/lists/*

RUN set -x; \
  ros -e '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' &&  \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/ci.conf"

RUN set -x; \
  ros install rove

ENTRYPOINT ["/bin/bash"]
