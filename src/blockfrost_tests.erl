-module(blockfrost_tests).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
  ok = blockfrost:setup().

setup1_test() ->
  ok = blockfrost:setup("mainnet1A2B3C4D5E6F7G8H9I0J1K2L3M4N5O6P").

blocks_test() ->
  blockfrost:setup(),
  {ok, _} = blockfrost:get_blocks_latest().
