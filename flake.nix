{
  description = "kudzu";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  };

  outputs = {
    self,
    nixpkgs,
  }:
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function rec {
          inherit system;
          compilerVersion = "ghc96";
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev: {
              # kudzu = hfinal.callCabal2nix "kudzu" ./. {};
              kudzu-core = hfinal.callCabal2nix "kudzu-core" ./kudzu-core/. {};
              kudzu-leancheck = hfinal.callCabal2nix "kudzu-leancheck" ./kudzu-leancheck/. {};

            };
          };
        });
    in
    {
      # nix fmt
      formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);
      allowBroken = true;
      # nix develop
      devShell = forAllSystems ({hsPkgs, pkgs, ...}:
        hsPkgs.shellFor {
          # withHoogle = true;
          packages = p: [
            p.kudzu-core
            p.kudzu-leancheck
          ];
          buildInputs = with pkgs;
            [
              hsPkgs.haskell-language-server
              haskellPackages.cabal-install
              cabal2nix
              haskellPackages.ghcid
              haskellPackages.fourmolu
              haskellPackages.cabal-fmt
            ]
            ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
        });

      # nix build
      packages = forAllSystems ({hsPkgs, ...}: {
        kudzu-core = hsPkgs.kudzu-core;
        kudzu-leancheck = hsPkgs.kudzu-leancheck;
          default = hsPkgs.kudzu;
      });

      # You can't build the kudzu package as a check because of IFD in cabal2nix
      checks = {};

      # nix run
      apps = forAllSystems ({system, ...}: {
        kudzu = {
          type = "app";
          program = "${self.packages.${system}.kudzu}/bin/kudzu";
        };
        default = self.apps.${system}.kudzu;
      });
    };
}
