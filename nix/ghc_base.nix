{ compilerVersion
, lock ? builtins.fromJSON (builtins.readFile ../flake.lock)
}:

let
  #nixpkgs_2 because that's the main nixpkgs in our flake.
  pkgs = import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs_2.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs_2.locked.narHash;
    })
    { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
