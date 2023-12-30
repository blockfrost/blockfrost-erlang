-module(blockfrost_try).

-export([test/1]).
-export([testrr/0]).
-export([testpartial/0]).
-export([get_latest_block_txs/0]).
-export([get_latest_block_txs/2]).

-include("blockfrost_core.hrl").
-include("blockfrost_try.hrl").

-spec test(string())
  -> {ok, jsx:json_term()} | error.
test(Project) ->
  blockfrost_core:setup(Project),
  blockfrost_core:perform_request("/").

testrr() ->
  #testr{str="str", uni = 12, wtf = pls, tup = {1, "a"}}.

add(A, B) -> A + B.

testpartial() ->
  Increment = fun(X) -> add(1, X) end,
  Increment(100).

get_latest_block_txs() ->
  get_latest_block_txs(#paged{}, #sort_order{}).

get_latest_block_txs(Paged, SortOrder) ->
  QS = [ { <<"count">>, Paged#paged.count_per_page }
       , { <<"page">>, Paged#paged.page_number }
       , { <<"order">>, SortOrder#sort_order.sort_order }
       ],
  blockfrost_core:perform_request("/blocks/latest/txs", QS).
