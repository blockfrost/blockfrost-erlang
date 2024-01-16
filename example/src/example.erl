-module(example).

-export([loop/0]).

loop() ->
  Res = blockfrost:get_blocks_latest(),
  case Res of
    {ok, Block} ->
      Hash = maps:get(<<"hash">>, Block),
      TxCount = maps:get(<<"tx_count">>, Block),
      io:format("Block ~p tx count: ~p~n", [binary:bin_to_list(Hash), TxCount]),
      timer:sleep(timer:seconds(5)),
      loop();
    _Else ->
      io:format("Got error ~p~n", [_Else])
  end.
