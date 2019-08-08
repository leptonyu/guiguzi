FROM ghc-8.6.5 as builder

ADD . /data

RUN cd /data \
 && cabal v2-update \
 && cabal v2-install main \
 && cp /root/.cabal/bin/guiguzi /data/main

FROM alpine:3.10
COPY --from=builder /main
ENTRYPOINT ["/main"] 
