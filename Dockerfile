FROM fukamachi/sbcl
ARG ULTRALISP_VERSION

WORKDIR /app

COPY . /root/.roswell/local-projects/fukamachi/qlot

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends \
    git \
    openssh-client && \
  rm -rf /var/lib/apt/lists/*

RUN set -x; \
  if [ "$ULTRALISP_VERSION" = "" ]; then \
    ros -e '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)'; \
  else \
    ros -e "(ql-dist:install-dist \"http://dist.ultralisp.org/ultralisp/$ULTRALISP_VERSION/distinfo.txt\" :prompt nil)"; \
  fi; \
  ros -S /root/.roswell/local-projects/fukamachi/qlot install qlot && \
  ros -e '(ql:quickload (list :qlot :qlot/cli :qlot/distify))'

ENTRYPOINT ["qlot"]
