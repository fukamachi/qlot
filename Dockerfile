ARG SBCL_VERSION=2.0.9
FROM fukamachi/sbcl:${SBCL_VERSION}

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends \
    git \
    openssh-client && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /root/.roswell/local-projects/qlot
RUN ros install qlot && ros -e '(ql:quickload :qlot/distify)'

ENTRYPOINT ["qlot"]
