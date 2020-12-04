FROM racket/racket:7.9-cs-full
WORKDIR /app
COPY app /app
RUN raco pkg install \
    --no-docs --auto \
    chk \
    html-parsing \
    http-easy
RUN raco exe -o /app/linkcheck /app/cmdline.rkt
ENTRYPOINT [ "/app/linkcheck" ]