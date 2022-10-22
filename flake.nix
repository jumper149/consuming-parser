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
      let previousOutput = incremental.packages.x86_64-linux.incremental.incremental;
      in
      (pkgs.haskell.lib.overrideCabal self.packages.x86_64-linux.default
        (drv: {
          preBuild = pkgs.lib.optionalString (previousOutput != null) ''
            mkdir -p dist/build
            tar xzf ${previousOutput}/dist.tar.gz -C dist/build
          '';
          postInstall = ''
            mkdir $incremental
            tar czf $incremental/dist.tar.gz -C dist/build --mtime='1970-01-01T00:00:00Z' .
          '';
          preFixup = ''
            # Don't try to strip incremental build outputs
            outputs=(${"\\" + "\${"}outputs[@]/incremental})
          '';
        })
      ).overrideAttrs (finalAttrs: previousAttrs: {
        outputs = previousAttrs.outputs ++ ["incremental"];
      });

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
