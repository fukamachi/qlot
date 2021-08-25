FROM fukamachi/sbcl

WORKDIR /app

COPY . /root/.roswell/local-projects/fukamachi/qlot

RUN set -x; \
  ros -S /root/.roswell/local-projects/fukamachi/qlot install qlot && \
  ros -e '(ql:quickload (list :qlot :qlot/cli :qlot/distify))'

ENTRYPOINT ["qlot"]
