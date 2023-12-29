{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  name = "blockfrost-erlang shell";
  buildInputs = with pkgs; [
    erlang
    rebar3
  ];

  shellHook = ''
    echo ""
    echo "# blockfrost-erlang shell"
    echo ""
    echo "## Build with"
    echo "rebar3 compile"

    export ERL_AFLAGS="+pc unicode -kernel shell_history enabled -enable-feature all"
    echo "## Shell"
    echo "rebar3 shell"

    echo "## Dialyzer"
    echo "rebar3 dialyzer"
  '';
}

