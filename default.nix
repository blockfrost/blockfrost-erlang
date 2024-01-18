{ pkgs ? import <nixpkgs> { } }:
with pkgs.beamPackages;
buildRebar3 {
  name = "blockfrost_erlang";
  version = "0.1.1";
  src = ./.;
  profile = "prod";
  beamDeps = builtins.attrValues
    (import ./rebar-deps.nix {
      inherit (pkgs) fetchHex fetchgit fetchFromGitHub;
      builder = buildRebar3;
  });
}
