FROM scratch
COPY bin/guiguzi /main
ENTRYPOINT ["/main"] 