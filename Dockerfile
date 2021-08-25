FROM clfoundation/sbcl

WORKDIR /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp
COPY . /root/common-lisp/qlot

RUN set -x; \
  sbcl --load /root/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  sbcl --no-userinit \
    --eval '(require (quote asdf))' \
    --eval '(load #P"/root/quicklisp/setup.lisp")' \
    --eval '(ql:quickload (list :qlot/cli :qlot/distify))' --quit && \
  echo "#!/bin/sh\nexec sbcl --noinform --non-interactive --load /root/quicklisp/setup.lisp --eval '(ql:quickload :qlot/cli :silent t)' --eval '(apply (function qlot/cli::qlot-command) (rest sb-ext:*posix-argv*))' \"\$@\"\n" > /usr/local/bin/qlot && \
  chmod u+x /usr/local/bin/qlot && \
  rm /root/quicklisp.lisp

ENTRYPOINT ["qlot"]
