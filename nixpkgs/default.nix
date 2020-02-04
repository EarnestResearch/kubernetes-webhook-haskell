{ system ? builtins.currentSystem }:
let
  sources = import ../nix/sources.nix;
  erNix = import sources.er-nix;
in
erNix.pkgsForSystem system
