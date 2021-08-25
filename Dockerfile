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
    --eval '(ql:quickload :qlot/distify)' --quit && \
  sbcl --eval '(asdf:make :qlot)' && \
  mv /root/common-lisp/qlot/qlot /usr/local/bin && \
  rm /root/quicklisp.lisp

ENTRYPOINT ["qlot"]
