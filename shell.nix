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
  '';
}

