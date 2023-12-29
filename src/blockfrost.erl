-module(blockfrost).

-export([init/0]).
-export([setup/1]).
-export([performRequest/1]).
-export([performRequest/3]).
-export_type([network/0]).
-export_type([error/0]).

-export([lookupConfig/0]).

-include("blockfrost.hrl").

-type network() ::
     mainnet
   | preprod
   | preview
   | sanchonet
   | ipfs
   .

-spec parseNetwork(string())
  -> {ok, network()} | {error, string(), string()}.
parseNetwork(NetString) ->
  case NetString of
    "mainnet" -> {ok, mainnet};
    "preprod" -> {ok, preprod};
    "preview" -> {ok, preview};
    "sanchonet" -> {ok, sanchonet};
    "ipfs" -> {ok, ipfs};
    _Else -> {error, "No such network", NetString}
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

-define(domain, "blockfrost.io").

-spec renderURL(string())
  -> string().
renderURL(URL) ->
  "https://" ++
  case lookupConfig() of
    {ok, _Token, Net} -> renderNetwork(Net);
    _Else -> _Else
  end
  ++ "."
  ++ ?domain
  ++ "/api/v0/"
  ++ URL
  .

-spec setup(string())
  -> ok.
setup(Project) ->
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

-spec performRequest(string())
  -> {ok, jsx:json_term()} | error.
performRequest(URL) ->
  performRequest(URL, get, <<>>).

-spec performRequest(string(), term(), term())
  -> {ok, jsx:json_term()} | error.
performRequest(URL, Method, Payload) ->
  {ok, Token, _Net} = lookupConfig(),
  {ok, Ver} = application:get_key(blockfrost_erlang, vsn),
  Headers = [ {<<"project_id">>, Token}
            , {<<"User-agent">>, "blockfrost-erlang/" ++ Ver}
            ],
  Options = [],
  FullURL = renderURL(URL),

  case hackney:request(Method, FullURL, Headers, Payload, Options) of
    {ok, 200, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      {ok, jsx:decode(Body)};
    {ok, Err, _, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      ErrBody = jsx:decode(Body),
      ErrRec = #error
                  { error = maps:get(<<"error">>, ErrBody, "")
                  , message = maps:get(<<"message">>, ErrBody, "")
                  , status_code = maps:get(<<"status_code">>, ErrBody, 200)
                  },
      case Err of
        429 ->
          timer:sleep(timer:minutes(5)),
          performRequest(URL);
        _Else -> ErrRec
      end;
    _Else -> _Else
  end.

% due to application:get_key(blockfrost_erlang, vsn)
% which returns any()
-dialyzer({[no_return], [performRequest/1, performRequest/3]}).
