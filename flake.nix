{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    incremental = {
      type = "github";
      owner = "jumper149";
      repo = "consuming-parser";
      ref = "incremental";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, incremental }: {

    overlays.default = final: prev: {
      haskellPackages = prev.haskell.packages.ghc942.extend (haskellFinal: haskellPrev: { # TODO: Using GHC 9.4.2.
      });
    };

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "consuming-parser" src {};

    packages.x86_64-linux.incremental =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      with import ./nix/haskell/lib.nix { lib = pkgs.lib; haskellLib = pkgs.haskell.lib; };
      buildIncrementally {
        regularPackage = self.packages.x86_64-linux.default;
        previousIncrement = incremental.packages.x86_64-linux.incremental;
      };

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
