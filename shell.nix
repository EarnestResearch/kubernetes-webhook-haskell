let
  pkgs = import ./nixpkgs {};
  hsPkgs = import ./default.nix { inherit pkgs; };
in
hsPkgs.shellFor {
  packages = ps: with ps; [ kubernetes-webhook-haskell ];
  buildInputs = with pkgs; [
    fd
    git
    hlint
    ormolu
    stack
  ];

  inherit (pkgs.earnestresearch.pre-commit-check) shellHook;
}
