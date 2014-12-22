let
  filterHaskellSource = builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist");
in
{ pkgs ? import <nixpkgs> { }
, src ? filterHaskellSource ./.
, kraken ? import ../kraken {}
}:
pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "kraken-web";
  args = { inherit kraken; };
  cabalDrvArgs = {
     buildTools = [ pkgs.graphviz pkgs.file ];
  };
}
