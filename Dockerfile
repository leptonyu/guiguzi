FROM icymint/ghc:8.6.5-deps as builder

ADD . /data

RUN cd /data \
 && cabal v2-update \
 && cabal v2-install main --disable-tests

FROM scratch
COPY --from=builder /root/.cabal/bin/guiguzi /main
ENTRYPOINT ["/main"] 
