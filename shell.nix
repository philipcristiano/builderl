let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;

  # Get from github
  ghpkgs = pkgs.callPackage (fetchTarball https://github.com/philipcristiano/dev-env/archive/v0.0.12.tar.gz) {};

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ ghpkgs.cfssl
                  pkgs.gnumake
                  pkgs.erlangR21
                ];
}
