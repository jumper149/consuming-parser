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
        graphmod = (haskellPrev.graphmod.overrideAttrs (oldAttrs: {
          src = prev.fetchFromGitHub {
            owner = "jumper149";
            repo = "graphmod";
            rev = "b684ce4d6af97179eccb65d2567d6165d43fa3e0";
            sha256 = "sha256-I5OfUGV9TbxLCyc8LhdZODhw5EpJXyXeFdaN7gMmhC8=";
          };
        }));
      });
    };

    packages.x86_64-linux.default =
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

    checks.x86_64-linux.hlint =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      stdenv.mkDerivation {
        name = "hlint"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          hlint ./src
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.hlint
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

    checks.x86_64-linux.graphmod =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
      stdenv.mkDerivation {
        name = "graphmod"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          graphmod > graphmod.out
          dot -Tdot graphmod.out > graphmod.dot
          dot -Tpdf graphmod.out > graphmod.pdf
        '';
        installPhase = ''
          mkdir $out
          cp graphmod.dot $out
          cp graphmod.pdf $out
        '';
        nativeBuildInputs = [
          haskellPackages.graphmod
          pkgs.graphviz
        ];
      };

    devShells.x86_64-linux.default =
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
          self.packages.x86_64-linux.default
        ];
        withHoogle = true;
      };

  };
}
