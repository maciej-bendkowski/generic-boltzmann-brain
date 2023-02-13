{
  description = "Generic Boltzmann Brain";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    paganini-hs.url = "github:maciej-bendkowski/paganini-hs/214013fdc8aa274d2e92d50484bc0f840bf77f18";
  };

  outputs =
    {
      self
    , nixpkgs
    , flake-utils
    , paganini-hs
    , ...
    }@inputs:
      flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let

        name = "generic-boltzmann-brain";
        compiler = "ghc925";

        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
        };

        optas = p: with p;
          (
            buildPythonPackage rec {
              pname = "optas";
              version = "1.0.3";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-92aaW9ZxvRy/dfYhw7IS+lfDAP2UuBuJhNDNTW7Xkzc=";
              };
              doCheck = false;
            }
          );

        paganini = p: with p;
          (
            buildPythonPackage rec {
              pname = "paganini";
              version = "1.5.0";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-hsDqONhBPQlgQHKBh5tdp/pzoXynUz7ztXXILrGgslo=";
              };
              doCheck = false;
              propagatedBuildInputs = [
                pkgs.python3Packages.numpy
                pkgs.python3Packages.sympy
                pkgs.python3Packages.scipy
                pkgs.python3Packages.networkx
                pkgs.python3Packages.cvxpy
                (optas p)
              ];
            }
          );

        pythonPackages = p: with p; [
          (optas p)
          (paganini p)
        ];

        haskellPackages = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: {
            "${name}" = (self.callCabal2nix "generic-boltzmann-brain" ./. {}).overrideAttrs(
              old: {
                nativeBuildInputs = old.nativeBuildInputs ++ [
                  (pkgs.python3.withPackages pythonPackages)
                ];
              }
            );

            paganini-hs = pkgs.haskell.lib.dontCheck (
              super.callCabal2nix "paganini-hs" paganini-hs {});
          };
        };

        devShell = haskellPackages.shellFor {
          withHoogle = true; # Provides docs, optional.
          packages = p: [
            p.generic-boltzmann-brain
          ];
          buildInputs = [
            haskellPackages.stack
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.fourmolu
            (pkgs.python3.withPackages pythonPackages)
          ];
        };

        defaultPackage = haskellPackages."${name}";
        derive = import ./.;

      in
        {
          inherit derive defaultPackage devShell;
        });
}
