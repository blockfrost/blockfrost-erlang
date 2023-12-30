-module(blockfrost).

-include("blockfrost_core.hrl").

-export([setup/0]).
-export([setup/1]).
-export([all_pages/1]).
-export_type([network/0]).
-export_type([error/0]).
-export_type([paged/0]).
-export_type([sort_order/0]).

%% Health
-export([get/0]).
-export([get_health/0]).
-export([get_health_clock/0]).

%% Metrics
-export([get_metrics/0]).
-export([get_metrics_endpoints/0]).

%% Cardano » Accounts
-export([get_accounts_by_stake_address/1]).
-export([get_accounts_by_stake_address_rewards/1, get_accounts_by_stake_address_rewards/3]).
-export([get_accounts_by_stake_address_history/1, get_accounts_by_stake_address_history/3]).
-export([get_accounts_by_stake_address_delegations/1, get_accounts_by_stake_address_delegations/3]).
-export([get_accounts_by_stake_address_registrations/1, get_accounts_by_stake_address_registrations/3]).
-export([get_accounts_by_stake_address_withdrawals/1, get_accounts_by_stake_address_withdrawals/3]).
-export([get_accounts_by_stake_address_mirs/1, get_accounts_by_stake_address_mirs/3]).
-export([get_accounts_by_stake_address_addresses/1, get_accounts_by_stake_address_addresses/3]).
-export([get_accounts_by_stake_address_addresses_assets/1, get_accounts_by_stake_address_addresses_assets/3]).
-export([get_accounts_by_stake_address_addresses_total/1]).

%% Cardano » Addresses
-export([get_addresses_by_address/1]).
-export([get_addresses_by_address_extended/1]).
-export([get_addresses_by_address_total/1]).
-export([get_addresses_by_address_utxos/1, get_addresses_by_address_utxos/3]).
-export([get_addresses_by_address_utxos_by_asset/2, get_addresses_by_address_utxos_by_asset/4]).
-export([get_addresses_by_address_transactions/1, get_addresses_by_address_transactions/3]).

%% Cardano » Assets
-export([get_assets/0, get_assets/2]).
-export([get_assets_by_asset/1]).
-export([get_assets_by_asset_history/1, get_assets_by_asset_history/3]).
-export([get_assets_by_asset_transactions/1, get_assets_by_asset_transactions/3]).
-export([get_assets_by_asset_addresses/1, get_assets_by_asset_addresses/3]).
-export([get_assets_policy_by_policy_id/1, get_assets_policy_by_policy_id/3]).

%% Cardano » Blocks
-export([get_blocks_latest/0]).
-export([get_blocks_latest_txs/0, get_blocks_latest_txs/2]).
-export([get_blocks_by_hash_or_number/1]).
-export([get_blocks_slot_by_slot_number/1]).
-export([get_blocks_epoch_by_epoch_number_slot_by_slot_number/2]).
-export([get_blocks_by_hash_or_number_next/1, get_blocks_by_hash_or_number_next/2]).
-export([get_blocks_by_hash_or_number_previous/1, get_blocks_by_hash_or_number_previous/2]).
-export([get_blocks_by_hash_or_number_txs/1, get_blocks_by_hash_or_number_txs/3]).
-export([get_blocks_by_hash_or_number_addresses/1, get_blocks_by_hash_or_number_addresses/2]).

%% Cardano » Epochs
-export([get_epochs_latest/0]).
-export([get_epochs_latest_parameters/0]).
-export([get_epochs_by_epoch_number/1]).
-export([get_epochs_by_epoch_number_next/1, get_epochs_by_epoch_number_next/2]).
-export([get_epochs_by_epoch_number_previous/1, get_epochs_by_epoch_number_previous/2]).
-export([get_epochs_by_epoch_number_stakes/1, get_epochs_by_epoch_number_stakes/2]).
-export([get_epochs_by_epoch_number_stakes_by_pool_id/2, get_epochs_by_epoch_number_stakes_by_pool_id/3]).
-export([get_epochs_by_epoch_number_blocks/1, get_epochs_by_epoch_number_blocks/3]).
-export([get_epochs_by_epoch_number_blocks_by_pool_id/2, get_epochs_by_epoch_number_blocks_by_pool_id/4]).
-export([get_epochs_by_epoch_number_parameters/1]).

%% Cardano » Ledger
-export([get_genesis/0]).

%% Cardano » Metadata
-export([get_metadata_txs_labels/0, get_metadata_txs_labels/2]).
-export([get_metadata_txs_labels_by_label/1, get_metadata_txs_labels_by_label/3]).
-export([get_metadata_txs_labels_by_label_cbor/1, get_metadata_txs_labels_by_label_cbor/3]).

%% Cardano » Network
-export([get_network/0]).
-export([get_network_eras/0]).

%% Cardano » Pools
-export([get_pools/0, get_pools/2]).
-export([get_pools_extended/0, get_pools_extended/2]).
-export([get_pools_retired/0, get_pools_retired/2]).
-export([get_pools_retiring/0, get_pools_retiring/2]).
-export([get_pools_by_pool_id/1]).
-export([get_pools_by_pool_id_history/1, get_pools_by_pool_id_history/3]).
-export([get_pools_by_pool_id_metadata/1]).
-export([get_pools_by_pool_id_relays/1]).
-export([get_pools_by_pool_id_delegators/1, get_pools_by_pool_id_delegators/3]).
-export([get_pools_by_pool_id_blocks/1, get_pools_by_pool_id_blocks/3]).
-export([get_pools_by_pool_id_updates/1, get_pools_by_pool_id_updates/3]).

%% Cardano » Scripts
-export([get_scripts/0, get_scripts/2]).
-export([get_scripts_by_script_hash/1]).
-export([get_scripts_by_script_hash_redeemers/1, get_scripts_by_script_hash_redeemers/3]).
-export([get_scripts_datum_by_datum_hash/1]).
-export([get_scripts_datum_by_datum_hash_cbor/1]).
-export([get_scripts_by_script_hash_json/1]).
-export([get_scripts_by_script_hash_cbor/1]).

%% Cardano » Transactions
-export([get_txs_by_hash/1]).
-export([get_txs_by_hash_utxos/1]).
-export([get_txs_by_hash_redeemers/1]).
-export([get_txs_by_hash_stakes/1]).
-export([get_txs_by_hash_delegations/1]).
-export([get_txs_by_hash_withdrawals/1]).
-export([get_txs_by_hash_mirs/1]).
-export([get_txs_by_hash_pool_updates/1]).
-export([get_txs_by_hash_pool_retires/1]).
-export([get_txs_by_hash_metadata/1]).
-export([get_txs_by_hash_metadata_cbor/1]).
-export([post_tx_submit/1]).

%% Cardano » Utilities
-export([get_utils_addresses_xpub_by_xpub_by_role_by_index/3]).
-export([post_utils_txs_evaluate/1]).
-export([post_utils_txs_evaluate_utxos/1]).

%% IPFS » Add
-export([post_ipfs_add/1]).

%% IPFS » Gateway
-export([get_ipfs_gateway_by_IPFS_path/1]).

%% IPFS » Pins
-export([post_ipfs_pin_add_by_IPFS_path/1]).
-export([get_ipfs_pin_list/0, get_ipfs_pin_list/2]).
-export([get_ipfs_pin_list_by_IPFS_path/1]).
-export([post_ipfs_pin_remove_by_IPFS_path/1]).

%% Nut.link
-export([get_nutlink_by_address/1]).
-export([get_nutlink_by_address_tickers/1, get_nutlink_by_address_tickers/3]).
-export([get_nutlink_by_address_tickers_by_ticker/2, get_nutlink_by_address_tickers_by_ticker/4]).
-export([get_nutlink_tickers_by_ticker/1, get_nutlink_tickers_by_ticker/3]).

% @doc Setup Blockfrost API client using BLOCKFROST_TOKEN_PATH
% environment variable
-spec setup()
  -> ok | {error, string()}.
setup() ->
  blockfrost_core:setup().

% @doc Setup Blockfrost API client from string
% containing Blockfrost project token, i.e. `mainnet1A2B3C4D5E6F7G8H9I0J1K2L3M4N5O6P'
-spec setup(string())
  -> ok | {error, string()}.
setup(Project) ->
  blockfrost_core:setup(Project).

% @doc Query all results, until we get less than maximum items per page.
% Usage: blockfrost:all_pages(fun(P) -> blockfrost:get_latest_block_txs(P, #sort_order{}) end).
-spec all_pages(fun((paged()) -> any()))
  -> {ok, any()} | error.
all_pages(F) ->
  blockfrost_core:all_pages(F).

%% Health

%% @doc Root endpoint
%% Root endpoint has no other function than to point end users to documentation.
%% Endpoint `/'
-spec get()
    -> {ok, jsx:json_term()} | error.
get() ->
  URL = io_lib:format("/", []),
  blockfrost_core:perform_request(URL).

%% @doc Backend health status
%% Return backend status as a boolean. Your application should handle situations when backend for the given chain is unavailable.
%% Endpoint `/health'
-spec get_health()
    -> {ok, jsx:json_term()} | error.
get_health() ->
  URL = io_lib:format("/health", []),
  blockfrost_core:perform_request(URL).

%% @doc Current backend time
%% This endpoint provides the current UNIX time. Your application might use this to verify if the client clock is not out of sync.
%% Endpoint `/health/clock'
-spec get_health_clock()
    -> {ok, jsx:json_term()} | error.
get_health_clock() ->
  URL = io_lib:format("/health/clock", []),
  blockfrost_core:perform_request(URL).

%% Metrics

%% @doc Blockfrost usage metrics
%% History of your Blockfrost usage metrics in the past 30 days.
%% Endpoint `/metrics'
-spec get_metrics()
    -> {ok, jsx:json_term()} | error.
get_metrics() ->
  URL = io_lib:format("/metrics", []),
  blockfrost_core:perform_request(URL).

%% @doc Blockfrost endpoint usage metrics
%% History of your Blockfrost usage metrics per endpoint in the past 30 days.
%% Endpoint `/metrics/endpoints'
-spec get_metrics_endpoints()
    -> {ok, jsx:json_term()} | error.
get_metrics_endpoints() ->
  URL = io_lib:format("/metrics/endpoints", []),
  blockfrost_core:perform_request(URL).

%% Cardano » Accounts

%% @doc Specific account address
%% Obtain information about a specific stake account.
%% Endpoint `/accounts/{stake_address}'
-spec get_accounts_by_stake_address(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address(Stake_address) ->
  URL = io_lib:format("/accounts/~p", [Stake_address]),
  blockfrost_core:perform_request(URL).

%% @doc Specific reward history
%% Obtain information about the reward history of a specific account.
%% Endpoint `/accounts/{stake_address}/rewards'
-spec get_accounts_by_stake_address_rewards(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_rewards(Stake_address) ->
  get_accounts_by_stake_address_rewards(Stake_address, #paged{}, #sort_order{}).

%% @doc Specific reward history
%% Obtain information about the reward history of a specific account.
%% Endpoint `/accounts/{stake_address}/rewards'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_rewards(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_rewards(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/rewards", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Account history
%% Obtain information about the history of a specific account.
%% Endpoint `/accounts/{stake_address}/history'
-spec get_accounts_by_stake_address_history(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_history(Stake_address) ->
  get_accounts_by_stake_address_history(Stake_address, #paged{}, #sort_order{}).

%% @doc Account history
%% Obtain information about the history of a specific account.
%% Endpoint `/accounts/{stake_address}/history'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_history(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_history(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/history", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Account delegation history
%% Obtain information about the delegation of a specific account.
%% Endpoint `/accounts/{stake_address}/delegations'
-spec get_accounts_by_stake_address_delegations(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_delegations(Stake_address) ->
  get_accounts_by_stake_address_delegations(Stake_address, #paged{}, #sort_order{}).

%% @doc Account delegation history
%% Obtain information about the delegation of a specific account.
%% Endpoint `/accounts/{stake_address}/delegations'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_delegations(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_delegations(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/delegations", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Account registration history
%% Obtain information about the registrations and deregistrations of a specific account.
%% Endpoint `/accounts/{stake_address}/registrations'
-spec get_accounts_by_stake_address_registrations(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_registrations(Stake_address) ->
  get_accounts_by_stake_address_registrations(Stake_address, #paged{}, #sort_order{}).

%% @doc Account registration history
%% Obtain information about the registrations and deregistrations of a specific account.
%% Endpoint `/accounts/{stake_address}/registrations'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_registrations(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_registrations(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/registrations", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Account withdrawal history
%% Obtain information about the withdrawals of a specific account.
%% Endpoint `/accounts/{stake_address}/withdrawals'
-spec get_accounts_by_stake_address_withdrawals(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_withdrawals(Stake_address) ->
  get_accounts_by_stake_address_withdrawals(Stake_address, #paged{}, #sort_order{}).

%% @doc Account withdrawal history
%% Obtain information about the withdrawals of a specific account.
%% Endpoint `/accounts/{stake_address}/withdrawals'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_withdrawals(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_withdrawals(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/withdrawals", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Account MIR history
%% Obtain information about the MIRs of a specific account.
%% Endpoint `/accounts/{stake_address}/mirs'
-spec get_accounts_by_stake_address_mirs(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_mirs(Stake_address) ->
  get_accounts_by_stake_address_mirs(Stake_address, #paged{}, #sort_order{}).

%% @doc Account MIR history
%% Obtain information about the MIRs of a specific account.
%% Endpoint `/accounts/{stake_address}/mirs'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_mirs(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_mirs(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/mirs", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Account associated addresses
%% Obtain information about the addresses of a specific account.
%% Endpoint `/accounts/{stake_address}/addresses'
-spec get_accounts_by_stake_address_addresses(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_addresses(Stake_address) ->
  get_accounts_by_stake_address_addresses(Stake_address, #paged{}, #sort_order{}).

%% @doc Account associated addresses
%% Obtain information about the addresses of a specific account.
%% Endpoint `/accounts/{stake_address}/addresses'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_addresses(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_addresses(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/addresses", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Assets associated with the account addresses
%% Obtain information about assets associated with addresses of a specific account.
%% Endpoint `/accounts/{stake_address}/addresses/assets'
-spec get_accounts_by_stake_address_addresses_assets(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_addresses_assets(Stake_address) ->
  get_accounts_by_stake_address_addresses_assets(Stake_address, #paged{}, #sort_order{}).

%% @doc Assets associated with the account addresses
%% Obtain information about assets associated with addresses of a specific account.
%% Endpoint `/accounts/{stake_address}/addresses/assets'
%% Variant supporting paging and sorting
-spec get_accounts_by_stake_address_addresses_assets(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_addresses_assets(Stake_address, Paged, SortOrder) ->
  URL = io_lib:format("/accounts/~p/addresses/assets", [Stake_address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Detailed information about account associated addresses
%% Obtain summed details about all addresses associated with a given account. Be careful, as an account could be part of a mangled address and does not necessarily mean the addresses are owned by user as the account.
%% Endpoint `/accounts/{stake_address}/addresses/total'
-spec get_accounts_by_stake_address_addresses_total(string())
    -> {ok, jsx:json_term()} | error.
get_accounts_by_stake_address_addresses_total(Stake_address) ->
  URL = io_lib:format("/accounts/~p/addresses/total", [Stake_address]),
  blockfrost_core:perform_request(URL).

%% Cardano » Addresses

%% @doc Specific address
%% Obtain information about a specific address.
%% Endpoint `/addresses/{address}'
-spec get_addresses_by_address(string())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address(Address) ->
  URL = io_lib:format("/addresses/~p", [Address]),
  blockfrost_core:perform_request(URL).

%% @doc Specific address - extended
%% Obtain extended information about a specific address.
%% Endpoint `/addresses/{address}/extended'
-spec get_addresses_by_address_extended(string())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_extended(Address) ->
  URL = io_lib:format("/addresses/~p/extended", [Address]),
  blockfrost_core:perform_request(URL).

%% @doc Address details
%% Obtain details about an address.
%% Endpoint `/addresses/{address}/total'
-spec get_addresses_by_address_total(string())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_total(Address) ->
  URL = io_lib:format("/addresses/~p/total", [Address]),
  blockfrost_core:perform_request(URL).

%% @doc Address UTXOs
%% UTXOs of the address.
%% Endpoint `/addresses/{address}/utxos'
-spec get_addresses_by_address_utxos(string())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_utxos(Address) ->
  get_addresses_by_address_utxos(Address, #paged{}, #sort_order{}).

%% @doc Address UTXOs
%% UTXOs of the address.
%% Endpoint `/addresses/{address}/utxos'
%% Variant supporting paging and sorting
-spec get_addresses_by_address_utxos(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_utxos(Address, Paged, SortOrder) ->
  URL = io_lib:format("/addresses/~p/utxos", [Address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Address UTXOs of a given asset
%% UTXOs of the address.
%% Endpoint `/addresses/{address}/utxos/{asset}'
-spec get_addresses_by_address_utxos_by_asset(string(), string())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_utxos_by_asset(Address, Asset) ->
  get_addresses_by_address_utxos_by_asset(Address, Asset, #paged{}, #sort_order{}).

%% @doc Address UTXOs of a given asset
%% UTXOs of the address.
%% Endpoint `/addresses/{address}/utxos/{asset}'
%% Variant supporting paging and sorting
-spec get_addresses_by_address_utxos_by_asset(string(), string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_utxos_by_asset(Address, Asset, Paged, SortOrder) ->
  URL = io_lib:format("/addresses/~p/utxos/~p", [Address, Asset]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Address transactions
%% Transactions on the address.
%% Endpoint `/addresses/{address}/transactions'
-spec get_addresses_by_address_transactions(string())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_transactions(Address) ->
  get_addresses_by_address_transactions(Address, #paged{}, #sort_order{}).

%% @doc Address transactions
%% Transactions on the address.
%% Endpoint `/addresses/{address}/transactions'
%% Variant supporting paging and sorting
-spec get_addresses_by_address_transactions(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_addresses_by_address_transactions(Address, Paged, SortOrder) ->
  URL = io_lib:format("/addresses/~p/transactions", [Address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% Cardano » Assets

%% @doc Assets
%% List of assets.
%% Endpoint `/assets'
-spec get_assets()
    -> {ok, jsx:json_term()} | error.
get_assets() ->
  get_assets(#paged{}, #sort_order{}).

%% @doc Assets
%% List of assets.
%% Endpoint `/assets'
%% Variant supporting paging and sorting
-spec get_assets(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_assets(Paged, SortOrder) ->
  URL = io_lib:format("/assets", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Specific asset
%% Information about a specific asset.
%% Endpoint `/assets/{asset}'
-spec get_assets_by_asset(string())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset(Asset) ->
  URL = io_lib:format("/assets/~p", [Asset]),
  blockfrost_core:perform_request(URL).

%% @doc Asset history
%% History of a specific asset.
%% Endpoint `/assets/{asset}/history'
-spec get_assets_by_asset_history(string())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset_history(Asset) ->
  get_assets_by_asset_history(Asset, #paged{}, #sort_order{}).

%% @doc Asset history
%% History of a specific asset.
%% Endpoint `/assets/{asset}/history'
%% Variant supporting paging and sorting
-spec get_assets_by_asset_history(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset_history(Asset, Paged, SortOrder) ->
  URL = io_lib:format("/assets/~p/history", [Asset]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Asset transactions
%% List of a specific asset transactions
%% Endpoint `/assets/{asset}/transactions'
-spec get_assets_by_asset_transactions(string())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset_transactions(Asset) ->
  get_assets_by_asset_transactions(Asset, #paged{}, #sort_order{}).

%% @doc Asset transactions
%% List of a specific asset transactions
%% Endpoint `/assets/{asset}/transactions'
%% Variant supporting paging and sorting
-spec get_assets_by_asset_transactions(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset_transactions(Asset, Paged, SortOrder) ->
  URL = io_lib:format("/assets/~p/transactions", [Asset]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Asset addresses
%% List of a addresses containing a specific asset
%% Endpoint `/assets/{asset}/addresses'
-spec get_assets_by_asset_addresses(string())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset_addresses(Asset) ->
  get_assets_by_asset_addresses(Asset, #paged{}, #sort_order{}).

%% @doc Asset addresses
%% List of a addresses containing a specific asset
%% Endpoint `/assets/{asset}/addresses'
%% Variant supporting paging and sorting
-spec get_assets_by_asset_addresses(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_assets_by_asset_addresses(Asset, Paged, SortOrder) ->
  URL = io_lib:format("/assets/~p/addresses", [Asset]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Assets of a specific policy
%% List of asset minted under a specific policy.
%% Endpoint `/assets/policy/{policy_id}'
-spec get_assets_policy_by_policy_id(string())
    -> {ok, jsx:json_term()} | error.
get_assets_policy_by_policy_id(Policy_id) ->
  get_assets_policy_by_policy_id(Policy_id, #paged{}, #sort_order{}).

%% @doc Assets of a specific policy
%% List of asset minted under a specific policy.
%% Endpoint `/assets/policy/{policy_id}'
%% Variant supporting paging and sorting
-spec get_assets_policy_by_policy_id(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_assets_policy_by_policy_id(Policy_id, Paged, SortOrder) ->
  URL = io_lib:format("/assets/policy/~p", [Policy_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% Cardano » Blocks

%% @doc Latest block
%% Return the latest block available to the backends, also known as the tip of the blockchain.
%% Endpoint `/blocks/latest'
-spec get_blocks_latest()
    -> {ok, jsx:json_term()} | error.
get_blocks_latest() ->
  URL = io_lib:format("/blocks/latest", []),
  blockfrost_core:perform_request(URL).

%% @doc Latest block transactions
%% Return the transactions within the latest block.
%% Endpoint `/blocks/latest/txs'
-spec get_blocks_latest_txs()
    -> {ok, jsx:json_term()} | error.
get_blocks_latest_txs() ->
  get_blocks_latest_txs(#paged{}, #sort_order{}).

%% @doc Latest block transactions
%% Return the transactions within the latest block.
%% Endpoint `/blocks/latest/txs'
%% Variant supporting paging and sorting
-spec get_blocks_latest_txs(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_blocks_latest_txs(Paged, SortOrder) ->
  URL = io_lib:format("/blocks/latest/txs", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Latest block transactions
%% Return the transactions within the latest block.
%% Endpoint `/blocks/{hash_or_number}'
-spec get_blocks_by_hash_or_number(integer() | string())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number(Hash_or_number) ->
  URL = io_lib:format("/blocks/~p", [Hash_or_number]),
  blockfrost_core:perform_request(URL).

%% @doc Specific block in a slot
%% Return the content of a requested block for a specific slot.
%% Endpoint `/blocks/slot/{slot_number}'
-spec get_blocks_slot_by_slot_number(integer())
    -> {ok, jsx:json_term()} | error.
get_blocks_slot_by_slot_number(Slot_number) ->
  URL = io_lib:format("/blocks/slot/~p", [Slot_number]),
  blockfrost_core:perform_request(URL).

%% @doc Specific block in a slot in an epoch
%% Return the content of a requested block for a specific slot in an epoch.
%% Endpoint `/blocks/epoch/{epoch_number}/slot/{slot_number}'
-spec get_blocks_epoch_by_epoch_number_slot_by_slot_number(integer(), integer())
    -> {ok, jsx:json_term()} | error.
get_blocks_epoch_by_epoch_number_slot_by_slot_number(Epoch_number, Slot_number) ->
  URL = io_lib:format("/blocks/epoch/~p/slot/~p", [Epoch_number, Slot_number]),
  blockfrost_core:perform_request(URL).

%% @doc Listing of next blocks
%% Return the list of blocks following a specific block.
%% Endpoint `/blocks/{hash_or_number}/next'
-spec get_blocks_by_hash_or_number_next(integer() | string())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_next(Hash_or_number) ->
  get_blocks_by_hash_or_number_next(Hash_or_number, #paged{}).

%% @doc Listing of next blocks
%% Return the list of blocks following a specific block.
%% Endpoint `/blocks/{hash_or_number}/next'
%% Variant supporting paging
-spec get_blocks_by_hash_or_number_next(integer() | string(), paged())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_next(Hash_or_number, Paged) ->
  URL = io_lib:format("/blocks/~p/next", [Hash_or_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Listing of preious blocks
%% Return the list of blocks preceeding a specific block.
%% Endpoint `/blocks/{hash_or_number}/previous'
-spec get_blocks_by_hash_or_number_previous(integer() | string())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_previous(Hash_or_number) ->
  get_blocks_by_hash_or_number_previous(Hash_or_number, #paged{}).

%% @doc Listing of preious blocks
%% Return the list of blocks preceeding a specific block.
%% Endpoint `/blocks/{hash_or_number}/previous'
%% Variant supporting paging
-spec get_blocks_by_hash_or_number_previous(integer() | string(), paged())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_previous(Hash_or_number, Paged) ->
  URL = io_lib:format("/blocks/~p/previous", [Hash_or_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Block transactions
%% Return the transactions within the block.
%% Endpoint `/blocks/{hash_or_number}/txs'
-spec get_blocks_by_hash_or_number_txs(integer() | string())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_txs(Hash_or_number) ->
  get_blocks_by_hash_or_number_txs(Hash_or_number, #paged{}, #sort_order{}).

%% @doc Block transactions
%% Return the transactions within the block.
%% Endpoint `/blocks/{hash_or_number}/txs'
%% Variant supporting paging and sorting
-spec get_blocks_by_hash_or_number_txs(integer() | string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_txs(Hash_or_number, Paged, SortOrder) ->
  URL = io_lib:format("/blocks/~p/txs", [Hash_or_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Addresses affected in a specific block
%% Return list of addresses affected in the specified block with additional information, sorted by the bech32 address, ascending.
%% Endpoint `/blocks/{hash_or_number}/addresses'
-spec get_blocks_by_hash_or_number_addresses(integer() | string())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_addresses(Hash_or_number) ->
  get_blocks_by_hash_or_number_addresses(Hash_or_number, #paged{}).

%% @doc Addresses affected in a specific block
%% Return list of addresses affected in the specified block with additional information, sorted by the bech32 address, ascending.
%% Endpoint `/blocks/{hash_or_number}/addresses'
%% Variant supporting paging
-spec get_blocks_by_hash_or_number_addresses(integer() | string(), paged())
    -> {ok, jsx:json_term()} | error.
get_blocks_by_hash_or_number_addresses(Hash_or_number, Paged) ->
  URL = io_lib:format("/blocks/~p/addresses", [Hash_or_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% Cardano » Epochs

%% @doc Latest epoch
%% Return the information about the latest, therefore current, epoch.
%% Endpoint `/epochs/latest'
-spec get_epochs_latest()
    -> {ok, jsx:json_term()} | error.
get_epochs_latest() ->
  URL = io_lib:format("/epochs/latest", []),
  blockfrost_core:perform_request(URL).

%% @doc Latest epoch protocol parameters
%% Return the protocol parameters for the latest epoch.
%% Endpoint `/epochs/latest/parameters'
-spec get_epochs_latest_parameters()
    -> {ok, jsx:json_term()} | error.
get_epochs_latest_parameters() ->
  URL = io_lib:format("/epochs/latest/parameters", []),
  blockfrost_core:perform_request(URL).

%% @doc Specific epoch
%% Return the content of the requested epoch.
%% Endpoint `/epochs/{epoch_number}'
-spec get_epochs_by_epoch_number(integer())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number(Epoch_number) ->
  URL = io_lib:format("/epochs/~p", [Epoch_number]),
  blockfrost_core:perform_request(URL).

%% @doc List of next epochs
%% Return the list of epochs following a specific epoch.
%% Endpoint `/epochs/{epoch_number}/next'
-spec get_epochs_by_epoch_number_next(integer())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_next(Epoch_number) ->
  get_epochs_by_epoch_number_next(Epoch_number, #paged{}).

%% @doc List of next epochs
%% Return the list of epochs following a specific epoch.
%% Endpoint `/epochs/{epoch_number}/next'
%% Variant supporting paging
-spec get_epochs_by_epoch_number_next(integer(), paged())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_next(Epoch_number, Paged) ->
  URL = io_lib:format("/epochs/~p/next", [Epoch_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc List of previous epochs
%% Return the list of epochs preceding a specific epoch.
%% Endpoint `/epochs/{epoch_number}/previous'
-spec get_epochs_by_epoch_number_previous(integer())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_previous(Epoch_number) ->
  get_epochs_by_epoch_number_previous(Epoch_number, #paged{}).

%% @doc List of previous epochs
%% Return the list of epochs preceding a specific epoch.
%% Endpoint `/epochs/{epoch_number}/previous'
%% Variant supporting paging
-spec get_epochs_by_epoch_number_previous(integer(), paged())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_previous(Epoch_number, Paged) ->
  URL = io_lib:format("/epochs/~p/previous", [Epoch_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Stake distribution
%% Return the active stake distribution for the specified epoch.
%% Endpoint `/epochs/{epoch_number}/stakes'
-spec get_epochs_by_epoch_number_stakes(integer())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_stakes(Epoch_number) ->
  get_epochs_by_epoch_number_stakes(Epoch_number, #paged{}).

%% @doc Stake distribution
%% Return the active stake distribution for the specified epoch.
%% Endpoint `/epochs/{epoch_number}/stakes'
%% Variant supporting paging
-spec get_epochs_by_epoch_number_stakes(integer(), paged())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_stakes(Epoch_number, Paged) ->
  URL = io_lib:format("/epochs/~p/stakes", [Epoch_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Stake distribution by pool
%% Return the active stake distribution for the epoch specified by stake pool.
%% Endpoint `/epochs/{epoch_number}/stakes/{pool_id}'
-spec get_epochs_by_epoch_number_stakes_by_pool_id(integer(), string())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_stakes_by_pool_id(Epoch_number, Pool_id) ->
  get_epochs_by_epoch_number_stakes_by_pool_id(Epoch_number, Pool_id, #paged{}).

%% @doc Stake distribution by pool
%% Return the active stake distribution for the epoch specified by stake pool.
%% Endpoint `/epochs/{epoch_number}/stakes/{pool_id}'
%% Variant supporting paging
-spec get_epochs_by_epoch_number_stakes_by_pool_id(integer(), string(), paged())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_stakes_by_pool_id(Epoch_number, Pool_id, Paged) ->
  URL = io_lib:format("/epochs/~p/stakes/~p", [Epoch_number, Pool_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Block distribution
%% Return the blocks minted for the epoch specified.
%% Endpoint `/epochs/{epoch_number}/blocks'
-spec get_epochs_by_epoch_number_blocks(integer())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_blocks(Epoch_number) ->
  get_epochs_by_epoch_number_blocks(Epoch_number, #paged{}, #sort_order{}).

%% @doc Block distribution
%% Return the blocks minted for the epoch specified.
%% Endpoint `/epochs/{epoch_number}/blocks'
%% Variant supporting paging and sorting
-spec get_epochs_by_epoch_number_blocks(integer(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_blocks(Epoch_number, Paged, SortOrder) ->
  URL = io_lib:format("/epochs/~p/blocks", [Epoch_number]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Block distribution by pool
%% Return the block minted for the epoch specified by stake pool.
%% Endpoint `/epochs/{epoch_number}/blocks/{pool_id}'
-spec get_epochs_by_epoch_number_blocks_by_pool_id(integer(), string())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_blocks_by_pool_id(Epoch_number, Pool_id) ->
  get_epochs_by_epoch_number_blocks_by_pool_id(Epoch_number, Pool_id, #paged{}, #sort_order{}).

%% @doc Block distribution by pool
%% Return the block minted for the epoch specified by stake pool.
%% Endpoint `/epochs/{epoch_number}/blocks/{pool_id}'
%% Variant supporting paging and sorting
-spec get_epochs_by_epoch_number_blocks_by_pool_id(integer(), string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_blocks_by_pool_id(Epoch_number, Pool_id, Paged, SortOrder) ->
  URL = io_lib:format("/epochs/~p/blocks/~p", [Epoch_number, Pool_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Protocol parameters
%% Return the protocol parameters for the specified epoch.
%% Endpoint `/epochs/{epoch_number}/parameters'
-spec get_epochs_by_epoch_number_parameters(integer())
    -> {ok, jsx:json_term()} | error.
get_epochs_by_epoch_number_parameters(Epoch_number) ->
  URL = io_lib:format("/epochs/~p/parameters", [Epoch_number]),
  blockfrost_core:perform_request(URL).

%% Cardano » Ledger

%% @doc Blockchain genesis
%% Return the information about blockchain genesis.
%% Endpoint `/genesis'
-spec get_genesis()
    -> {ok, jsx:json_term()} | error.
get_genesis() ->
  URL = io_lib:format("/genesis", []),
  blockfrost_core:perform_request(URL).

%% Cardano » Metadata

%% @doc Transaction metadata labels
%% List of all used transaction metadata labels.
%% Endpoint `/metadata/txs/labels'
-spec get_metadata_txs_labels()
    -> {ok, jsx:json_term()} | error.
get_metadata_txs_labels() ->
  get_metadata_txs_labels(#paged{}, #sort_order{}).

%% @doc Transaction metadata labels
%% List of all used transaction metadata labels.
%% Endpoint `/metadata/txs/labels'
%% Variant supporting paging and sorting
-spec get_metadata_txs_labels(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_metadata_txs_labels(Paged, SortOrder) ->
  URL = io_lib:format("/metadata/txs/labels", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Transaction metadata content in JSON
%% Transaction metadata per label.
%% Endpoint `/metadata/txs/labels/{label}'
-spec get_metadata_txs_labels_by_label(string())
    -> {ok, jsx:json_term()} | error.
get_metadata_txs_labels_by_label(Label) ->
  get_metadata_txs_labels_by_label(Label, #paged{}, #sort_order{}).

%% @doc Transaction metadata content in JSON
%% Transaction metadata per label.
%% Endpoint `/metadata/txs/labels/{label}'
%% Variant supporting paging and sorting
-spec get_metadata_txs_labels_by_label(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_metadata_txs_labels_by_label(Label, Paged, SortOrder) ->
  URL = io_lib:format("/metadata/txs/labels/~p", [Label]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Transaction metadata content in CBOR
%% Transaction metadata per label.
%% Endpoint `/metadata/txs/labels/{label}/cbor'
-spec get_metadata_txs_labels_by_label_cbor(string())
    -> {ok, jsx:json_term()} | error.
get_metadata_txs_labels_by_label_cbor(Label) ->
  get_metadata_txs_labels_by_label_cbor(Label, #paged{}, #sort_order{}).

%% @doc Transaction metadata content in CBOR
%% Transaction metadata per label.
%% Endpoint `/metadata/txs/labels/{label}/cbor'
%% Variant supporting paging and sorting
-spec get_metadata_txs_labels_by_label_cbor(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_metadata_txs_labels_by_label_cbor(Label, Paged, SortOrder) ->
  URL = io_lib:format("/metadata/txs/labels/~p/cbor", [Label]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% Cardano » Network

%% @doc Network information
%% Return detailed network information.
%% Endpoint `/network'
-spec get_network()
    -> {ok, jsx:json_term()} | error.
get_network() ->
  URL = io_lib:format("/network", []),
  blockfrost_core:perform_request(URL).

%% @doc Query summary of blockchain eras
%% Returns start and end of each era along with parameters that can vary between hard forks.
%% Endpoint `/network/eras'
-spec get_network_eras()
    -> {ok, jsx:json_term()} | error.
get_network_eras() ->
  URL = io_lib:format("/network/eras", []),
  blockfrost_core:perform_request(URL).

%% Cardano » Pools

%% @doc List of stake pools
%% List of registered stake pools.
%% Endpoint `/pools'
-spec get_pools()
    -> {ok, jsx:json_term()} | error.
get_pools() ->
  get_pools(#paged{}, #sort_order{}).

%% @doc List of stake pools
%% List of registered stake pools.
%% Endpoint `/pools'
%% Variant supporting paging and sorting
-spec get_pools(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools(Paged, SortOrder) ->
  URL = io_lib:format("/pools", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc List of stake pools with additional information
%% List of registered stake pools with additional information.
%% Endpoint `/pools/extended'
-spec get_pools_extended()
    -> {ok, jsx:json_term()} | error.
get_pools_extended() ->
  get_pools_extended(#paged{}, #sort_order{}).

%% @doc List of stake pools with additional information
%% List of registered stake pools with additional information.
%% Endpoint `/pools/extended'
%% Variant supporting paging and sorting
-spec get_pools_extended(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_extended(Paged, SortOrder) ->
  URL = io_lib:format("/pools/extended", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc List of retired stake pools
%% List of already retired stake pools.
%% Endpoint `/pools/retired'
-spec get_pools_retired()
    -> {ok, jsx:json_term()} | error.
get_pools_retired() ->
  get_pools_retired(#paged{}, #sort_order{}).

%% @doc List of retired stake pools
%% List of already retired stake pools.
%% Endpoint `/pools/retired'
%% Variant supporting paging and sorting
-spec get_pools_retired(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_retired(Paged, SortOrder) ->
  URL = io_lib:format("/pools/retired", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc List of retiring stake pools
%% List of stake pools retiring in the upcoming epochs
%% Endpoint `/pools/retiring'
-spec get_pools_retiring()
    -> {ok, jsx:json_term()} | error.
get_pools_retiring() ->
  get_pools_retiring(#paged{}, #sort_order{}).

%% @doc List of retiring stake pools
%% List of stake pools retiring in the upcoming epochs
%% Endpoint `/pools/retiring'
%% Variant supporting paging and sorting
-spec get_pools_retiring(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_retiring(Paged, SortOrder) ->
  URL = io_lib:format("/pools/retiring", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Specific stake pool
%% Pool information.
%% Endpoint `/pools/{pool_id}'
-spec get_pools_by_pool_id(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id(Pool_id) ->
  URL = io_lib:format("/pools/~p", [Pool_id]),
  blockfrost_core:perform_request(URL).

%% @doc Stake pool history
%% History of stake pool parameters over epochs.
%% Endpoint `/pools/{pool_id}/history'
-spec get_pools_by_pool_id_history(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_history(Pool_id) ->
  get_pools_by_pool_id_history(Pool_id, #paged{}, #sort_order{}).

%% @doc Stake pool history
%% History of stake pool parameters over epochs.
%% Endpoint `/pools/{pool_id}/history'
%% Variant supporting paging and sorting
-spec get_pools_by_pool_id_history(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_history(Pool_id, Paged, SortOrder) ->
  URL = io_lib:format("/pools/~p/history", [Pool_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Stake pool metadata
%% Stake pool registration metadata.
%% Endpoint `/pools/{pool_id}/metadata'
-spec get_pools_by_pool_id_metadata(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_metadata(Pool_id) ->
  URL = io_lib:format("/pools/~p/metadata", [Pool_id]),
  blockfrost_core:perform_request(URL).

%% @doc Stake pool relays
%% Relays of a stake pool.
%% Endpoint `/pools/{pool_id}/relays'
-spec get_pools_by_pool_id_relays(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_relays(Pool_id) ->
  URL = io_lib:format("/pools/~p/relays", [Pool_id]),
  blockfrost_core:perform_request(URL).

%% @doc Stake pool delegators
%% List of current stake pools delegators.
%% Endpoint `/pools/{pool_id}/delegators'
-spec get_pools_by_pool_id_delegators(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_delegators(Pool_id) ->
  get_pools_by_pool_id_delegators(Pool_id, #paged{}, #sort_order{}).

%% @doc Stake pool delegators
%% List of current stake pools delegators.
%% Endpoint `/pools/{pool_id}/delegators'
%% Variant supporting paging and sorting
-spec get_pools_by_pool_id_delegators(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_delegators(Pool_id, Paged, SortOrder) ->
  URL = io_lib:format("/pools/~p/delegators", [Pool_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Stake pool blocks
%% List of stake pool blocks.
%% Endpoint `/pools/{pool_id}/blocks'
-spec get_pools_by_pool_id_blocks(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_blocks(Pool_id) ->
  get_pools_by_pool_id_blocks(Pool_id, #paged{}, #sort_order{}).

%% @doc Stake pool blocks
%% List of stake pool blocks.
%% Endpoint `/pools/{pool_id}/blocks'
%% Variant supporting paging and sorting
-spec get_pools_by_pool_id_blocks(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_blocks(Pool_id, Paged, SortOrder) ->
  URL = io_lib:format("/pools/~p/blocks", [Pool_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Stake pool updates
%% List of certificate updates to the stake pool.
%% Endpoint `/pools/{pool_id}/updates'
-spec get_pools_by_pool_id_updates(string())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_updates(Pool_id) ->
  get_pools_by_pool_id_updates(Pool_id, #paged{}, #sort_order{}).

%% @doc Stake pool updates
%% List of certificate updates to the stake pool.
%% Endpoint `/pools/{pool_id}/updates'
%% Variant supporting paging and sorting
-spec get_pools_by_pool_id_updates(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_pools_by_pool_id_updates(Pool_id, Paged, SortOrder) ->
  URL = io_lib:format("/pools/~p/updates", [Pool_id]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% Cardano » Scripts

%% @doc Scripts
%% List of scripts.
%% Endpoint `/scripts'
-spec get_scripts()
    -> {ok, jsx:json_term()} | error.
get_scripts() ->
  get_scripts(#paged{}, #sort_order{}).

%% @doc Scripts
%% List of scripts.
%% Endpoint `/scripts'
%% Variant supporting paging and sorting
-spec get_scripts(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_scripts(Paged, SortOrder) ->
  URL = io_lib:format("/scripts", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Specific scripts
%% Information about a specific script.
%% Endpoint `/scripts/{script_hash}'
-spec get_scripts_by_script_hash(string())
    -> {ok, jsx:json_term()} | error.
get_scripts_by_script_hash(Script_hash) ->
  URL = io_lib:format("/scripts/~p", [Script_hash]),
  blockfrost_core:perform_request(URL).

%% @doc Redeemers of a specific script
%% List of redeemers of a specific script.
%% Endpoint `/scripts/{script_hash}/redeemers'
-spec get_scripts_by_script_hash_redeemers(string())
    -> {ok, jsx:json_term()} | error.
get_scripts_by_script_hash_redeemers(Script_hash) ->
  get_scripts_by_script_hash_redeemers(Script_hash, #paged{}, #sort_order{}).

%% @doc Redeemers of a specific script
%% List of redeemers of a specific script.
%% Endpoint `/scripts/{script_hash}/redeemers'
%% Variant supporting paging and sorting
-spec get_scripts_by_script_hash_redeemers(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_scripts_by_script_hash_redeemers(Script_hash, Paged, SortOrder) ->
  URL = io_lib:format("/scripts/~p/redeemers", [Script_hash]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Datum value
%% Query JSON value of a datum by its hash
%% Endpoint `/scripts/datum/{datum_hash}'
-spec get_scripts_datum_by_datum_hash(string())
    -> {ok, jsx:json_term()} | error.
get_scripts_datum_by_datum_hash(Datum_hash) ->
  URL = io_lib:format("/scripts/datum/~p", [Datum_hash]),
  blockfrost_core:perform_request(URL).

%% @doc Datum CBOR value
%% Query CBOR serialised datum by its hash
%% Endpoint `/scripts/datum/{datum_hash}/cbor'
-spec get_scripts_datum_by_datum_hash_cbor(string())
    -> {ok, jsx:json_term()} | error.
get_scripts_datum_by_datum_hash_cbor(Datum_hash) ->
  URL = io_lib:format("/scripts/datum/~p/cbor", [Datum_hash]),
  blockfrost_core:perform_request(URL).

%% @doc Script JSON
%% JSON representation of a `timelock` script
%% Endpoint `/scripts/{script_hash}/json'
-spec get_scripts_by_script_hash_json(string())
    -> {ok, jsx:json_term()} | error.
get_scripts_by_script_hash_json(Script_hash) ->
  URL = io_lib:format("/scripts/~p/json", [Script_hash]),
  blockfrost_core:perform_request(URL).

%% @doc Script CBOR
%% CBOR representation of a `plutus` script
%% Endpoint `/scripts/{script_hash}/cbor'
-spec get_scripts_by_script_hash_cbor(string())
    -> {ok, jsx:json_term()} | error.
get_scripts_by_script_hash_cbor(Script_hash) ->
  URL = io_lib:format("/scripts/~p/cbor", [Script_hash]),
  blockfrost_core:perform_request(URL).

%% Cardano » Transactions

%% @doc Specific transaction
%% Return content of the requested transaction.
%% Endpoint `/txs/{hash}'
-spec get_txs_by_hash(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash(Hash) ->
  URL = io_lib:format("/txs/~p", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction UTXOs
%% Return the inputs and UTXOs of the specific transaction.
%% Endpoint `/txs/{hash}/utxos'
-spec get_txs_by_hash_utxos(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_utxos(Hash) ->
  URL = io_lib:format("/txs/~p/utxos", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction redeemers
%% Obtain the transaction redeemers.
%% Endpoint `/txs/{hash}/redeemers'
-spec get_txs_by_hash_redeemers(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_redeemers(Hash) ->
  URL = io_lib:format("/txs/~p/redeemers", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction stake addresses certificates 
%% Obtain information about (de)registration of stake addresses within a transaction.
%% Endpoint `/txs/{hash}/stakes'
-spec get_txs_by_hash_stakes(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_stakes(Hash) ->
  URL = io_lib:format("/txs/~p/stakes", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction delegation certificates
%% Obtain information about delegation certificates of a specific transaction.
%% Endpoint `/txs/{hash}/delegations'
-spec get_txs_by_hash_delegations(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_delegations(Hash) ->
  URL = io_lib:format("/txs/~p/delegations", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction withdrawal
%% Obtain information about withdrawals of a specific transaction.
%% Endpoint `/txs/{hash}/withdrawals'
-spec get_txs_by_hash_withdrawals(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_withdrawals(Hash) ->
  URL = io_lib:format("/txs/~p/withdrawals", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction MIRs
%% Obtain information about Move Instantaneous Rewards (MIRs) of a specific transaction.
%% Endpoint `/txs/{hash}/mirs'
-spec get_txs_by_hash_mirs(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_mirs(Hash) ->
  URL = io_lib:format("/txs/~p/mirs", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction stake pool registration and update certificates
%% Obtain information about stake pool registration and update certificates of a specific transaction.
%% Endpoint `/txs/{hash}/pool_updates'
-spec get_txs_by_hash_pool_updates(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_pool_updates(Hash) ->
  URL = io_lib:format("/txs/~p/pool_updates", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction stake pool retirement certificates
%% Obtain information about stake pool retirements within a specific transaction.
%% Endpoint `/txs/{hash}/pool_retires'
-spec get_txs_by_hash_pool_retires(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_pool_retires(Hash) ->
  URL = io_lib:format("/txs/~p/pool_retires", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction metadata
%% Obtain the transaction metadata.
%% Endpoint `/txs/{hash}/metadata'
-spec get_txs_by_hash_metadata(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_metadata(Hash) ->
  URL = io_lib:format("/txs/~p/metadata", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Transaction metadata in CBOR
%% Obtain the transaction metadata in CBOR.
%% Endpoint `/txs/{hash}/metadata/cbor'
-spec get_txs_by_hash_metadata_cbor(string())
    -> {ok, jsx:json_term()} | error.
get_txs_by_hash_metadata_cbor(Hash) ->
  URL = io_lib:format("/txs/~p/metadata/cbor", [Hash]),
  blockfrost_core:perform_request(URL).

%% @doc Submit a transaction
%% Submit an already serialized transaction to the network.
%% Endpoint `/tx/submit'
-spec post_tx_submit(term())
    -> {ok, jsx:json_term()} | error.
post_tx_submit(Body) ->
  URL = io_lib:format("/tx/submit", []),
  blockfrost_core:perform_request(URL, [], post, Body).

%% Cardano » Utilities

%% @doc Derive an address
%% Derive Shelley address from an xpub.
%% Endpoint `/utils/addresses/xpub/{xpub}/{role}/{index}'
-spec get_utils_addresses_xpub_by_xpub_by_role_by_index(string(), integer(), integer())
    -> {ok, jsx:json_term()} | error.
get_utils_addresses_xpub_by_xpub_by_role_by_index(Xpub, Role, Index) ->
  URL = io_lib:format("/utils/addresses/xpub/~p/~p/~p", [Xpub, Role, Index]),
  blockfrost_core:perform_request(URL).

%% @doc Submit a transaction for execution units evaluation
%% Submit an already serialized transaction to evaluate how much execution units it requires.
%% Endpoint `/utils/txs/evaluate'
-spec post_utils_txs_evaluate(term())
    -> {ok, jsx:json_term()} | error.
post_utils_txs_evaluate(Body) ->
  URL = io_lib:format("/utils/txs/evaluate", []),
  blockfrost_core:perform_request(URL, [], post, Body).

%% @doc Submit a transaction for execution units evaluation (additional UTXO set)
%% Submit a JSON payload with transaction CBOR and additional UTXO set to evaluate how much execution units it requires.
%% Endpoint `/utils/txs/evaluate/utxos'
-spec post_utils_txs_evaluate_utxos(term())
    -> {ok, jsx:json_term()} | error.
post_utils_txs_evaluate_utxos(Body) ->
  URL = io_lib:format("/utils/txs/evaluate/utxos", []),
  blockfrost_core:perform_request(URL, [], post, Body).

%% IPFS » Add

%% @doc Add a file or directory to IPFS
%% You need to `/ipfs/pin/add` an object to avoid it being garbage collected. This usage is being counted in your user account quota.
%% Endpoint `/ipfs/add'
-spec post_ipfs_add(term())
    -> {ok, jsx:json_term()} | error.
post_ipfs_add(Body) ->
  URL = io_lib:format("/ipfs/add", []),
  blockfrost_core:perform_request(URL, [], post, Body).

%% IPFS » Gateway

%% @doc Relay to an IPFS gateway
%% Retrieve an object from the IFPS gateway. (Useful if you do not want to rely on a public gateway, such as ``ipfs.blockfrost.dev`).
%% Endpoint `/ipfs/gateway/{IPFS_path}'
-spec get_ipfs_gateway_by_IPFS_path(string())
    -> {ok, jsx:json_term()} | error.
get_ipfs_gateway_by_IPFS_path(Ipfs_path) ->
  URL = io_lib:format("/ipfs/gateway/~p", [Ipfs_path]),
  blockfrost_core:perform_request(URL).

%% IPFS » Pins

%% @doc Pin an object
%% Pinned objects are counted in your user storage quota.
%% Endpoint `/ipfs/pin/add/{IPFS_path}'
-spec post_ipfs_pin_add_by_IPFS_path(string())
    -> {ok, jsx:json_term()} | error.
post_ipfs_pin_add_by_IPFS_path(Ipfs_path) ->
  URL = io_lib:format("/ipfs/pin/add/~p", [Ipfs_path]),
  blockfrost_core:perform_request(URL, [], post, <<>>).

%% @doc List pinned objects
%% List objects pinned to local storage.
%% Endpoint `/ipfs/pin/list'
-spec get_ipfs_pin_list()
    -> {ok, jsx:json_term()} | error.
get_ipfs_pin_list() ->
  get_ipfs_pin_list(#paged{}, #sort_order{}).

%% @doc List pinned objects
%% List objects pinned to local storage.
%% Endpoint `/ipfs/pin/list'
%% Variant supporting paging and sorting
-spec get_ipfs_pin_list(paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_ipfs_pin_list(Paged, SortOrder) ->
  URL = io_lib:format("/ipfs/pin/list", []),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc Get pinned object details
%% Obtain inormation about specific pinned object.
%% Endpoint `/ipfs/pin/list/{IPFS_path}'
-spec get_ipfs_pin_list_by_IPFS_path(string())
    -> {ok, jsx:json_term()} | error.
get_ipfs_pin_list_by_IPFS_path(Ipfs_path) ->
  URL = io_lib:format("/ipfs/pin/list/~p", [Ipfs_path]),
  blockfrost_core:perform_request(URL).

%% @doc Remove pinned object from local storage
%% Remove pinned object from local storage
%% Endpoint `/ipfs/pin/remove/{IPFS_path}'
-spec post_ipfs_pin_remove_by_IPFS_path(string())
    -> {ok, jsx:json_term()} | error.
post_ipfs_pin_remove_by_IPFS_path(Ipfs_path) ->
  URL = io_lib:format("/ipfs/pin/remove/~p", [Ipfs_path]),
  blockfrost_core:perform_request(URL, [], post, <<>>).

%% Nut.link

%% @doc List metadata about specific address
%% List metadata about specific address
%% Endpoint `/nutlink/{address}'
-spec get_nutlink_by_address(string())
    -> {ok, jsx:json_term()} | error.
get_nutlink_by_address(Address) ->
  URL = io_lib:format("/nutlink/~p", [Address]),
  blockfrost_core:perform_request(URL).

%% @doc List tickers for a specific metadata oracle
%% List tickers for a specific metadata oracle
%% Endpoint `/nutlink/{address}/tickers'
-spec get_nutlink_by_address_tickers(string())
    -> {ok, jsx:json_term()} | error.
get_nutlink_by_address_tickers(Address) ->
  get_nutlink_by_address_tickers(Address, #paged{}, #sort_order{}).

%% @doc List tickers for a specific metadata oracle
%% List tickers for a specific metadata oracle
%% Endpoint `/nutlink/{address}/tickers'
%% Variant supporting paging and sorting
-spec get_nutlink_by_address_tickers(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_nutlink_by_address_tickers(Address, Paged, SortOrder) ->
  URL = io_lib:format("/nutlink/~p/tickers", [Address]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc List of records of a specific ticker
%% List of records of a specific ticker
%% Endpoint `/nutlink/{address}/tickers/{ticker}'
-spec get_nutlink_by_address_tickers_by_ticker(string(), string())
    -> {ok, jsx:json_term()} | error.
get_nutlink_by_address_tickers_by_ticker(Address, Ticker) ->
  get_nutlink_by_address_tickers_by_ticker(Address, Ticker, #paged{}, #sort_order{}).

%% @doc List of records of a specific ticker
%% List of records of a specific ticker
%% Endpoint `/nutlink/{address}/tickers/{ticker}'
%% Variant supporting paging and sorting
-spec get_nutlink_by_address_tickers_by_ticker(string(), string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_nutlink_by_address_tickers_by_ticker(Address, Ticker, Paged, SortOrder) ->
  URL = io_lib:format("/nutlink/~p/tickers/~p", [Address, Ticker]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

%% @doc List of records of a specific ticker
%% List of records of a specific ticker
%% Endpoint `/nutlink/tickers/{ticker}'
-spec get_nutlink_tickers_by_ticker(string())
    -> {ok, jsx:json_term()} | error.
get_nutlink_tickers_by_ticker(Ticker) ->
  get_nutlink_tickers_by_ticker(Ticker, #paged{}, #sort_order{}).

%% @doc List of records of a specific ticker
%% List of records of a specific ticker
%% Endpoint `/nutlink/tickers/{ticker}'
%% Variant supporting paging and sorting
-spec get_nutlink_tickers_by_ticker(string(), paged(), sort_order())
    -> {ok, jsx:json_term()} | error.
get_nutlink_tickers_by_ticker(Ticker, Paged, SortOrder) ->
  URL = io_lib:format("/nutlink/tickers/~p", [Ticker]),
  QS =
    [ { <<"count">>, Paged#paged.count_per_page }
    , { <<"page">>,  Paged#paged.page_number }
    , { <<"order">>, SortOrder#sort_order.sort_order }
    ],
  blockfrost_core:perform_request(URL, QS).

% due to application:get_key(blockfrost_erlang, vsn)
% which returns any()
-dialyzer({[no_return],
  [ post_tx_submit/1
  , post_utils_txs_evaluate/1
  , post_utils_txs_evaluate_utxos/1
  , post_ipfs_add/1
  , post_ipfs_pin_add_by_IPFS_path/1
  , post_ipfs_pin_remove_by_IPFS_path/1
  ]}).