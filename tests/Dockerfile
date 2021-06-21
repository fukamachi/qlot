FROM clfoundation/sbcl

WORKDIR /app

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends git && \
  rm -rf /var/lib/apt/lists/*

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp
RUN set -x; \
  sbcl --noinform --non-interactive --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp && \
  sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --quit && \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/ci.conf"

RUN set -x; \
  sbcl --eval '(ql:quickload :rove)'

ENTRYPOINT ["sbcl", "--noinform", "--non-interactive"]
CMD ["--eval", "(ql:quickload :rove)", "--eval", "(rove:run :qlot/tests)"]
