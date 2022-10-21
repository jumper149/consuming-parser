{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    overlays.default = final: prev: {
      haskellPackages = prev.haskell.packages.ghc924.extend (haskellFinal: haskellPrev: { # TODO: Using GHC 9.2.4.
      });
    };

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "consuming-parser" src {};

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          cabal-install
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.default
        ];
      };

  };
}
