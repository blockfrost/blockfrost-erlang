-module(blockfrost_tests).
-include_lib("blockfrost_core.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
  ok = blockfrost:setup().

setup1_test() ->
  ok = blockfrost:setup("mainnet1A2B3C4D5E6F7G8H9I0J1K2L3M4N5O6P").

integration_test() ->
  blockfrost:setup(),
  {ok, _} = blockfrost:get_version(),
  {ok, _} = blockfrost:get_health(),
  {ok, _} = blockfrost:get_health_clock(),

  {ok, _} = blockfrost:get_metrics(),
  {ok, _} = blockfrost:get_metrics_endpoints(),

  {ok, _} = blockfrost:get_assets(),

  Blk = 3300000,
  {ok, _} = blockfrost:get_blocks_latest(),
  {ok, _} = blockfrost:get_blocks_latest_txs(),
  {ok, _} = blockfrost:get_blocks_latest_txs(#paged{}, #sort_order{}),
  {ok, _} = blockfrost:get_blocks_latest_txs(#paged{page_number=1, count_per_page=1}, #sort_order{sort_order=desc}),
  {ok, _} = blockfrost:get_blocks_by_hash_or_number(1),
  {ok, _} = blockfrost:get_blocks_slot_by_slot_number(1),
  {ok, _} = blockfrost:get_blocks_epoch_by_epoch_number_slot_by_slot_number(1, 1),
  {ok, _} = blockfrost:get_blocks_by_hash_or_number_next(1),
  {ok, _} = blockfrost:get_blocks_by_hash_or_number_previous(2),
  {ok, [<<"6a869e575ee4b8a45c1d69f077b562904e7599a0b1183bae2b913e96eeb355bf">>]} 
          = blockfrost:get_blocks_by_hash_or_number_txs(Blk),
  {ok, _} = blockfrost:get_blocks_by_hash_or_number_addresses(Blk),

  {ok, _} = blockfrost:get_epochs_latest(),
  {ok, _} = blockfrost:get_epochs_latest_parameters(),
  {ok, _} = blockfrost:get_epochs_by_epoch_number(1),
  {ok, _} = blockfrost:get_epochs_by_epoch_number_next(1),
  {ok, _} = blockfrost:get_epochs_by_epoch_number_previous(2),

  {ok, _} = blockfrost:get_genesis(),

  {ok, _} = blockfrost:get_metadata_txs_labels(),

  {ok, _} = blockfrost:get_network(),
  {ok, _} = blockfrost:get_network_eras(),

  {ok, _} = blockfrost:get_pools(),
  {ok, _} = blockfrost:get_pools_extended(),
  {ok, _} = blockfrost:get_pools_retired(),
  {ok, _} = blockfrost:get_pools_retiring(),

  {ok, _} = blockfrost:get_scripts(),

  ok.

-dialyzer({[no_return], [ integration_test/0 ]}).
