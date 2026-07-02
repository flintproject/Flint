{
  description = "A flake for building Flint";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-26.05;

  inputs.clibsedml = {
    url = github:flintproject/clibsedml;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-libsbml = {
    url = github:flintproject/flint-libsbml;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-sundials = {
    url = github:flintproject/flint-sundials;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-soslib = {
    url = github:flintproject/flint-soslib;
    inputs.flint-sundials.follows = "flint-sundials";
    inputs.flint-libsbml.follows = "flint-libsbml";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, clibsedml, flint-libsbml, flint-sundials, flint-soslib }: let

    allSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

    forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f (import nixpkgs { inherit system; }));

  in {

    packages = forAllSystems (pkgs: with pkgs; {

      default = stdenv.mkDerivation {
        pname = "flint";
        version = "2.4.99";

        nativeBuildInputs = [
          autoreconfHook
          gnum4
          pkg-config
          boost178
          protobuf
          wxwidgets_3_2
        ];

        buildInputs = [
          protobuf
          boost178
          libmicrohttpd
          libxml2
          sqlite
          zeromq czmq
          wxwidgets_3_2
          flint-libsbml.packages.${system}.default
          flint-sundials.packages.${system}.default
          flint-soslib.packages.${system}.default
          clibsedml.packages.${system}.default
        ];

        src = ./source;

        configureFlags = [
          "--with-boost=${boost178.dev}"
          "--with-wxWidgets=${wxwidgets_3_2}"
        ];

        doCheck = true;

        enableParallelBuilding = true;
      };

    });

    devShells = forAllSystems (pkgs: with pkgs; let

      default-shell = mkShell {

        packages = [
          autoconf
          automake
          libtool
          pkg-config
          protobuf
          boost178
          wxwidgets_3_2
          libmicrohttpd
          libxml2
          sqlite
          zeromq czmq
          wxwidgets_3_2
          flint-libsbml.packages.${system}.default
          flint-sundials.packages.${system}.default
          flint-soslib.packages.${system}.default
          clibsedml.packages.${system}.default
        ];

      };

    in {

      default = default-shell;

    });

  };
}
