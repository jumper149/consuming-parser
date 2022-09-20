{
  description = "A parser with additional type safety by tracking guaranteed consumption";

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

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "consuming-parser" src {};

    checks.x86_64-linux.fourmolu =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      stdenv.mkDerivation {
        name = "fourmolu"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          fourmolu --cabal-default-extensions --mode check $(find src -name '*.hs')
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.fourmolu
        ];
      };

    checks.x86_64-linux.hie-yaml =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      stdenv.mkDerivation {
        name = "hie-yaml"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          diff --report-identical-files ./hie.yaml <(gen-hie)
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.implicit-hie
        ];
      };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          cabal-install
          fourmolu
          ghcid
          haskell-language-server
          hlint
          implicit-hie
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.defaultPackage.x86_64-linux
        ];
        withHoogle = true;
      };

  };
}
