FROM icymint/ghc:8.6.5-deps as builder

ADD . /data

RUN cd /data \
 && cabal v2-update \
 && cabal v2-install main --disable-tests \
 && cp /root/.cabal/bin/guiguzi /data/main \
 && chmod +x /data/main

FROM scratch
COPY --from=builder /data/main /main
ENTRYPOINT ["/main"] 
