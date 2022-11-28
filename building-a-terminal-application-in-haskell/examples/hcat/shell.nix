{ pkgs ? import <nixpkgs> {}}:
let
  hcat = import ./default.nix { inherit pkgs; };
  hsPkgs = pkgs.haskellPackages;
in hsPkgs.shellFor {
  packages = _ : [ hcat ];
  withHoogle = false;
  buildInputs = [
    hsPkgs.cabal-install
  ];
}
