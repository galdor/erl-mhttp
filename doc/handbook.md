% erl-mhttp

# Introduction
The erl-mhttp project is an Erlang HTTP 1.1 implementation.

# Interface
## Sending requests
The `mhttp:send_request/1` and `mhttp:send_request/2` functions are used to
send requests.

Example:
```erlang
mhttp:send_request(#{method => get, target => <<"http://example.com">>},
                   #{pool => default})
```

If it succeeds, a response is returned:

```erlang
{ok,#{body =>
          <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n    <meta "...>>,
      header =>
          [{<<"Age">>,<<"429753">>},
           {<<"Cache-Control">>,<<"max-age=604800">>},
           {<<"Content-Type">>,<<"text/html; charset=UTF-8">>},
           {<<"Date">>,<<"Sat, 22 Aug 2020 16:12:05 GMT">>},
           {<<"Etag">>,<<"\"3147526947+ident\"">>},
           {<<"Expires">>,<<"Sat, 29 Aug 2020 16:12:05 GMT">>},
           {<<"Last-Modified">>,<<"Thu, 17 Oct 2019 07:18:26 GMT">>},
           {<<"Server">>,<<"ECS (dcb/7EEE)">>},
           {<<"Vary">>,<<"Accept-Encoding">>},
           {<<"X-Cache">>,<<"HIT">>},
           {<<"Content-Length">>,<<"1256">>}],
      reason => <<"OK">>,status => 200,version => http_1_1}}
```

# Requests
**TODO**

## Request options
**TODO**

# Responses
**TODO**

# Client
A client is a single connection to a HTTP server. While it can be directly
used, it is strongly recommended to work with `mhttp:send_request` which
relies on pools.

## Options
The following client options are available:

- `host`: the hostname or IP address to connect to (default: `<<"localhost">>`).
- `port`: the port number to connect to (default: `80`).
- `transport`: the network transport, either `tcp` or `tls`.
- `tcp_options`: a list of `gen_tcp` client options to apply.
- `tls_options`: a list of `ssl` client options to apply (only used if the
  transport is `tls`).
- `header`: a set of default header fields used for all requests sent.

Note that for TLS connections, the client uses both the list of TCP options
and the list of TLS options.

# Pool
A pool is a set of HTTP clients. Pools can send requests to any destination
and will create a new client for each new destination. Connections are kept
alive and reused when possible.

## Configuration
Pools are created by `mhttp_pool_sup` supervisor based on the configuration of
the `mhttp` application. Pools are identified by an atom. Each pool process is
registered as `mhttp_pool_<id>` where `<id>` is its identifier. For example,
the process of the `default` pool is registered as `mhttp_pool_default`.

The `default` pool is created with default options if application
configuration does not contain a pool with that identifier.

The following example configures a pool named `example` where all clients will
use a specific user agent:

```erlang
[{mhttp,
  [{pools,
    #{example => #{client_options =>
                       #{header => [{<<"User-Agent">>, <<"Example/1.0">>}]}}}}]}].
```

## Options
The following pool options are available:

- `client_options`: the set of client options used for every client in the
  pool. Note that `host`, `port` and `transport` will be overridden for each
  connection.

## Usage
The `pool` request option is used to select which pool will be used to send
the request. If the option is not provided, the `default` pool is used.

# Server
A server handles incoming HTTP requests. Each server spawns a set of
connection acceptors.

## Options
The following server options are available:

- `address`: the inet address the server to listen on (default: `loopback`).
- `port`: the port number the server to listen on.
- `tcp_options`: a list of `gen_tcp` listen options to apply.
- `router_options`: a map of options to use for the router associated with the
  server.

## Configuration
Servers are created by `mhttp_server_sup` supervisor based on the
configuration of the `mhttp` application. Servers are identified by an
atom. Each server process is registered as `mhttp_server_<id>` where `<id>` is
its identifier. For example, the process of the `test` server would be
registered as `mhttp_server_test`.

The following example configures a server named `example`.

```erlang
[{mhttp,
  [{servers,
    #{example => #{address => loopback,
                   port => 8080}}}]}].
```

## Handling routes
The `mhttp:set_server_router/2` function is used to set the router associated
with a server.

For example, assuming that there is a server identified as `example`:
```erlang
Handler = fun (_Request, _Context) ->
              #{status => 200, body => <<"Hello world!\n">>}
          end,
Router = #{routes => [{<<"/">>, Handler}]},
mhttp:set_server_router(example, Router).
```

Until a router has been set, a server will return an `unavailable_service`
route. The default handler for this route returns a HTTP 503 response.

# Router
A router is a list of HTTP request patterns and request handlers. When a
request is received the router is used to find the first pattern matching the
request and to execute the corresponding action.

A router is a map containing the following fields:

- `routes`: a list of routes (mandatory).
- `default_route`: a route which will be returned if no route matches a
  request.

## Routes
Each route is a tuple `{Pattern, Handler}`.

**TODO example**

All new routes are added to the end of the route list.

## Patterns
**TODO**

## Handlers
**TODO**
