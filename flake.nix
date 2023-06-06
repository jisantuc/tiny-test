{
  description = "A reproducible Haskell env for tiny-test";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-darwin"; };
      cabal = pkgs.cabal-install;
      ghc = pkgs.haskellPackages.ghcWithPackages (p: [
        p.haskell-language-server
        p.rainbow
        p.random
        p.text
        p.validation
      ]);
    in
    {
      devShells.x86_64-darwin.default = pkgs.mkShell { 
        name = "platform-dev-shell";
        buildInputs = [ cabal ghc ];
      };
    };
}
