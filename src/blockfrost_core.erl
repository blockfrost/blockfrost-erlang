-module(blockfrost_core).

-export([init/0]).
-export([setup/0]).
-export([setup/1]).
-export([perform_request/1]).
-export([perform_request/2]).
-export([perform_request/4]).
-export([all_pages/1]).
-export_type([network/0]).
-export_type([error/0]).
-export_type([paged/0]).
-export_type([sort_order/0]).

-include("blockfrost_core.hrl").

-spec parseNetwork(string())
  -> {ok, network()} | {error, string()}.
parseNetwork(NetString) ->
  case NetString of
    "mainnet" -> {ok, mainnet};
    "preprod" -> {ok, preprod};
    "preview" -> {ok, preview};
    "sanchonet" -> {ok, sanchonet};
    "ipfs" -> {ok, ipfs};
    _Else -> {error, "No such network " ++ NetString}
  end.

-spec renderNetwork(network())
  -> string().
renderNetwork(NetString) ->
  case NetString of
    mainnet -> "cardano-mainnet";
    preprod -> "cardano-preprod";
    preview -> "cardano-preview";
    sanchonet -> "cardano-sanchonet";
    ipfs -> "ipfs"
  end.

-spec renderURL(string(), hackney:qs_vals())
  -> binary().
renderURL(Path, QS) ->
  BaseURL =
    "https://" ++
    case lookupConfig() of
      {ok, _Token, Net} -> renderNetwork(Net);
      _Else -> _Else
    end
    ++ "."
    ++ ?domain,

  P = "/api/v0/" ++ Path,

  hackney_url:make_url(list_to_binary(BaseURL), list_to_binary(P), QS).

% @doc Setup Blockfrost API client using BLOCKFROST_TOKEN_PATH
% environment variable
-spec setup()
  -> ok | {error, string()}.
setup() ->
  case os:getenv("BLOCKFROST_TOKEN_PATH") of
    false -> {error, "Set BLOCKFROST_TOKEN_PATH to point to a file with token"};
    File -> case file:read_file(File) of
              {ok, Binary} -> setup
                                ( binary:bin_to_list
                                  ( binary:replace(Binary, <<"\n">>, <<"">>)
                                  )
                                );
              _Else -> _Else
            end
  end.

% @doc Setup Blockfrost API client from string
% containing Blockfrost project token, i.e. `mainnet1A2B3C4D5E6F7G8H9I0J1K2L3M4N5O6P'
-spec setup(string())
  -> ok | {error, string()}.
setup(Project) ->
  application:ensure_all_started(hackney),
  StrippedProj = string:strip(Project),
  Token = string:reverse(string:slice(string:reverse(StrippedProj), 0, 32)),
  Env = string:reverse(string:slice(string:reverse(StrippedProj), 32, infinity)),
  case parseNetwork(Env) of
    {ok, Net} ->
      init(),
      ets:insert(?MODULE, {config, {Net, Token}}),
      ok;
    _Else -> _Else
  end.

lookupConfig() ->
  case ets:whereis(?MODULE) of
    undefined -> {error, "Use setup/1 to setup Blockfrost API client"};
    _Else ->
      case ets:lookup(?MODULE, config) of
        [] -> {error, "Use setup/1 to setup Blockfrost API client"};
        [{config, {Net, Token}}] -> {ok, Token, Net}
      end
  end.

-spec init()
  -> ok.
init() ->
  case ets:whereis(?MODULE) of
    undefined ->
      _ = ets:new(?MODULE, [named_table, set]),
      ok;
    _Else ->
      ok
  end.


-spec perform_request(string())
  -> {ok, jsx:json_term()} | error.
perform_request(URL) ->
  perform_request(URL, []).

-spec perform_request(string(), hackney_url:qs_vals())
  -> {ok, jsx:json_term()} | error.
perform_request(URL, QS) ->
  perform_request(URL, QS, get, <<>>).

-spec perform_request(string(), hackney_url:qs_vals(), term(), term())
  -> {ok, jsx:json_term()} | error.
perform_request(URL, QS, Method, Payload) ->
  {ok, Token, _Net} = lookupConfig(),
  {ok, Ver} = application:get_key(blockfrost_erlang, vsn),
  Headers = [ {<<"project_id">>, Token}
            , {<<"User-agent">>, "blockfrost-erlang/" ++ Ver}
            ],
  Options = [],
  FullURL = renderURL(URL, QS),
  ?LOG({performReq, FullURL}),

  case hackney:request(Method, FullURL, Headers, Payload, Options) of
    {ok, 200, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      {ok, jsx:decode(Body)};
    {ok, Err, _, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      ?LOG({gotError, Body}),
      ErrBody = jsx:decode(Body),
      ErrRec = #error
                  { error = maps:get(<<"error">>, ErrBody, "")
                  , message = maps:get(<<"message">>, ErrBody, "")
                  , status_code = maps:get(<<"status_code">>, ErrBody, 200)
                  },
      case Err of
        429 ->
          ?LOG({rateLimited}),
          timer:sleep(timer:minutes(5)),
          perform_request(URL);
        _Else -> ErrRec
      end;
    _Else -> _Else
  end.

% due to application:get_key(blockfrost_erlang, vsn)
% which returns any()
-dialyzer({[no_return],
          [ perform_request/1
          , perform_request/2
          , perform_request/4
          ]}).

% @doc Query all results, until we get less than maximum items per page.
% Usage: blockfrost:all_pages(fun(P) -> blockfrost:get_latest_block_txs(P, #sort_order{}) end).
-spec all_pages(fun((paged()) -> any()))
  -> {ok, any()} | error.
all_pages(F) ->
  all_pages(F, #paged{}).

-spec all_pages(fun((paged()) -> any()), paged())
  -> {ok, any()} | error.
all_pages(F, Page) ->
  case F(Page) of
    {ok, Results} when length(Results) < ?max_page_size ->
      {ok, Results};
    {ok, Results} ->
      case all_pages(F, Page#paged{page_number = Page#paged.page_number + 1}) of
        {ok, Next} -> {ok, Results ++ Next};
        _Else -> _Else
      end;
    _Else -> _Else
  end.
