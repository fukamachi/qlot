FROM clfoundation/sbcl

WORKDIR /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp
COPY . /root/common-lisp/qlot

RUN set -x; \
  sbcl --load /root/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit && \
  sbcl --noinform --non-interactive \
    --eval '(require (quote asdf))' \
    --eval '(load #P"/root/quicklisp/setup.lisp")' \
    --eval '(ql:quickload (list :qlot/main :qlot/cli :qlot/distify))' \
    --eval '(ql:bundle-systems (mapcar (function ql-dist:name) (ql-dist:installed-systems (ql-dist:dist "quicklisp"))) :to "/app/bundled-dists/")' && \
  sbcl --noinform --non-interactive \
    --load /root/quicklisp/setup.lisp \
    --load /app/bundled-dists/bundle.lisp \
    --eval '(ql:quickload (list :qlot/main :qlot/cli :qlot/distify))' && \
  echo "#!/bin/sh\nexec sbcl --noinform --non-interactive --load /app/bundled-dists/bundle.lisp --eval '(asdf:load-system :qlot/cli)' --eval '(apply (function qlot/cli::qlot-command) (rest sb-ext:*posix-argv*))' \"\$@\"\n" > /usr/local/bin/qlot && \
  chmod u+x /usr/local/bin/qlot && \
  rm /root/quicklisp.lisp && rm -rf /root/quicklisp && rm -rf "/root/.cache/common-lisp/sbcl-*/root/quicklisp"

ENTRYPOINT ["qlot"]
