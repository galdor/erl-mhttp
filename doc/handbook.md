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
- `connection_timeout`: the timeout for the initial connection to the server.
- `read_timeout`: the timeout of each read operation when reading a
  response. Note that the timeout used for write operations is configured in
  `tcp_options` with `send_timeout` as specified in the [Erlang
  documentation](https://erlang.org/doc/man/gen_tcp.html).
- `tcp_options`: a list of `gen_tcp` client options to apply if the transport
  is either `tcp` or `tls`.
- `tls_options`: a list of `ssl` client options to apply if the transport
  is either `tls`.
- `header`: a set of default header fields used for all requests sent.
- `compression`: accept compressed responses and automatically decompress
  them. The only compression scheme currently supported is `gzip`.
- `log_requests`: toggle request logging (default: `true`.

For TLS connections, the client uses both the list of TCP options and the list
of TLS options.

# Pool
A pool is a set of HTTP clients. Pools can send requests to any destination
and will create a new client for each new destination. Connections are kept
alive and reused when possible.

Pools can maintain multiple connections to a single destination. Destinations
are identified by a client key of the form `{Host, Port, Transport}`. When
multiple connections are available, pools return a random one.

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

Pools can also be created with the `mhttp_pool_sup:start_pool/2` function;
these pools are also handled by the `mhttp_pool_sup` supervisor.

## Options
The following pool options are available:

- `client_options`: the set of client options used for every client in the
  pool. Note that `host`, `port` and `transport` will be overridden for each
  connection.
- `max_connections_per_key`: the maximum number of connections allowed for
  each client key (default: `infinity`).
- `request_timeout`: the amount of time after which a request is aborted and
  considered a failure in milliseconds. Requests stopped that way return a
  `request_timeout` error.
- `use_netrc`: load and use credentials from the netrc file of the current
  user.

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
- `listen_options`: a list of `gen_tcp` listen options to apply.
- `nb_acceptors`: the number of connection acceptor processes to start
  (default: 5).
- `service_unavailable_handler`: the handler called when the server is not
  available, for example before it has been assigned a router. The default
  handler sends a HTTP 503 plain text response.
- `error_handler`: the error handler called when an error is signaled during
  execution of the original route handler. The default handler sends a HTTP
  500 plain text response containing information about the request received
  and the error which was signaled.
- `idle_timeout`: a time in milliseconds for each connection to wait for
  activity before closing (default: 10'000ms). The associated timer is reset
  each time the server receives data from the client.

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

Servers can also be created with the `mhttp_server_sup:start_server/2`
function; these servers are also handled by the `mhttp_server_sup` supervisor.

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

Until a router has been set, a server will return a `service_unavailable`
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
Each route is a tuple `{Pattern, Handler}` where `Pattern` is a construction
used to match requests, and `Handler` is a request handler.

**TODO example**

All new routes are added to the end of the route list.

## Patterns
**TODO**

## Handlers
A handler has one the following forms:
- A function of type `mhttp:handler_fun/0`. The function is called with the
  request and context as arguments and returns a response.
- A tuple `{router, Router, Options}` or `{router, Router}` (equivalent to
  `{router, Router, #{}}`). The router is used to recursively find a matching
  route. Options affect the behaviour of request matching; the following
  options are supported:
  - `strip_path_prefix`: a binary string removed from the beginning of the
    request target path before passing the request to the new router. Note
    that the modification of the request target path only applies to the
    matching process. The handler function which will be called will receive
    the original request and target.

### Error handlers
**TODO**

# Log events
## mhttp.request.in
Indicates that a HTTP request was received and processed.

Attributes:
- `address`: the IP address the request was received from as a string.
- `processing_time`: the time spend processing the request in microseconds as
  an integer.
- `server`: the identifier string of the mhttp server managing the connection
  the request was received on.
- `status`: the HTTP status of the response sent as an integer.

## mhttp.request.out
Indicates that a HTTP request was sent and a response received.

Attributes:
- `pool`: if the HTTP client was part of a mhttp pool, the identifier string
  of the pool.
- `processing_time`: the time spend sending the request and receiving the
  response in microseconds as an integer.
- `status`: the HTTP status of the response received as an integer.

# Media types
The `mhttp_media_type` module provide support for MIME media types. A media
type is represented as one of the following tuples:
- `{Type, Subtype}`
- `{Type, Subtype, Parameters}`.

Parameters are represented as a list of tuples of the form `{Name, Value}`.

Types, subtypes, parameter names and parameter values are all binaries.
