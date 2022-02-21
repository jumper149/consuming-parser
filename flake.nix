{
  description = "A parser with additional type safety by tracking guaranteed consumption";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    deriving-trans = {
      type = "github";
      owner = "jumper149";
      repo = "deriving-trans";
      ref = "v0.3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, deriving-trans }: {

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        src = nix-gitignore.gitignoreSource [] ./.;
        overlay = self: super: {
          deriving-trans = self.callCabal2nix "deriving-trans" deriving-trans.outPath {};
        };
      in (haskellPackages.extend overlay).callCabal2nix "consuming-parser" src {};

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          ghcid
          haskell-language-server
          hlint
          hnix
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
