-module(mhttp_netrc).

-export([lookup/1]).

-spec lookup(uri:host()) -> {ok, netrc:entry()} | error.
lookup(Host) ->
  Query = #{machine => Host},
  case netrc:search(Query) of
    [Entry | _] ->
      {ok, Entry};
    [] ->
      error
  end.
