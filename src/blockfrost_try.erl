-module(blockfrost_try).

-export([test/1]).
-export([testrr/0]).
-export([testpartial/0]).

-include("blockfrost-try.hrl").

-spec test(string())
  -> {ok, jsx:json_term()} | error.
test(Project) ->
  blockfrost_core:setup(Project),
  blockfrost_core:performRequest("/").

testrr() ->
  #testr{str="str", uni = 12, wtf = pls, tup = {1, "a"}}.

add(A, B) -> A + B.

testpartial() ->
  Increment = fun(X) -> add(1, X) end,
  Increment(100).
