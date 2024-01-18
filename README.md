[![Hex pm](http://img.shields.io/hexpm/v/blockfrost_erlang.svg?style=flat)](https://hex.pm/packages/blockfrost_erlang)
[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/blockfrost/blockfrost-erlang/ci.yaml?branch=master)](https://github.com/blockfrost/blockfrost-erlang/actions/workflows/ci.yaml)
[![Made by Five Binaries](https://img.shields.io/badge/made%20by-Five%20Binaries-darkviolet.svg?style=flat-square)](https://fivebinaries.com/)

<img src="https://blockfrost.io/images/logo.svg" width="250" align="right" height="90">

# blockfrost-erlang

<br/>

<p align="center">Erlang SDK for <a href="https://blockfrost.io">Blockfrost.io</a> API.</p>
<p align="center">
  <a href="#about">About</a> •
  <a href="#getting-started">Getting started</a> •
  <a href="#installation">Installation</a> •
  <a href="#usage">Usage</a>
</p>

<br/>

## About

Erlang SDK for the Blockfrost.io API.

## Getting started

To use this SDK, you first need to log in to [blockfrost.io](https://blockfrost.io), create your project and retrieve the API token.

<img src="https://i.imgur.com/smY12ro.png">

<br/>

## Installation

The SDK uses [rebar3](http://rebar3.org) for project management, [hackney](https://github.com/benoitc/hackney)
as its HTTP client and [jsx](https://github.com/talentdeficit/jsx/) for `JSON`
parsing.

You can either work directly with this repository or add this package as a dependency of your project using


```erlang
{deps, [
  {blockfrost_erlang, "1.0.0"}
  ]}
```

To enter a development shell, use `nix-shell`.

## Usage

```sh
git clone https://github.com/blockfrost/blockfrost-erlang
cd blockfrost-erlang
nix-shell
rebar3 shell
```

If you export `BLOCKFROST_TOKEN_PATH` environment variable
`blockfrost-erlang` will automatically load
and configure network and token:

```sh
export BLOCKFROST_TOKEN_PATH=~/.blockfrost.mainnet.token
```

Then in `eshell`:

```erlang
blockfrost:get_blocks_latest().

{ok, Block} = blockfrost:get_blocks_latest().
Hash = maps:get(<<"hash">>, Block).
TxCount = maps:get(<<"tx_count">>, Block).
io:format("Block ~p tx count: ~p~n", [binary:bin_to_list(Hash), TxCount]).
```

Alternatively you can use `setup/1` function
which accepts a project string, for example

```erlang
blockfrost:setup("mainnet1A2B3C4D5E6F7G8H9I0J1K2L3M4N5O6P").
```

### `IPFS`

Simple `IPFS` upload example of a raw string:

```erlang
blockfrost:post_ipfs_add({multipart, [{<<"Filename">>, <<"Hello Erlang">>}]}).
```

See [hackney](https://github.com/benoitc/hackney#send-a-body) documentation
for more examples of `multipart` payload format.

## Maintenance

### Testing

Export a token
```sh
export BLOCKFROST_TOKEN_PATH=~/.blockfrost.mainnet.token
```

and run

```sh
rebar3 eunit
```

### Typecheck

Run `rebar3 dialyzer`

### Updating `rebar-deps.nix`

Run `rebar3 nix lock`
