# 鬼谷子, A Haskell Application Project.


### Buildin functions 

- [boots](https://hackage.haskell.org/package/boots) Based
- Configuration load and reload, by [salak](https://hackage.haskell.org/package/salak)
- [Logger](https://hackage.haskell.org/package/monad-logger)
- Web
  - [Servant](https://hackage.haskell.org/package/servant-server) Based
  - [Swagger](https://hackage.haskell.org/package/servant-swagger)
  - [OpenTracing](https://github.com/leptonyu/guiguzi/tree/master/opentracing)
  - Actuators
    - Health Check, allow extensions.
    - Refresh Configuration, by [salak](https://hackage.haskell.org/package/salak).
    - Info, show application name and version.
    - Metrics, by [ekg-core](https://hackage.haskell.org/package/ekg-core).
    - Logger level modification.
    - TODO
- Database with health check
  - [PostgreSQL](https://hackage.haskell.org/package/persistent-postgresql)
  - [SQLite](https://hackage.haskell.org/package/persistent-sqlite)
- [Redis](https://hackage.haskell.org/package/hedis) with health check
- [Http Client](https://hackage.haskell.org/package/http-client)
- TODO