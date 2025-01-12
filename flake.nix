{
  description = "A flake for building Flint";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-24.11;

  inputs.clibsedml = {
    url = github:flintproject/clibsedml;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-libsbml = {
    url = github:flintproject/flint-libsbml/5f01e223b0c1be9e06d007718aec6ae1b9dab436;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-sundials = {
    url = github:flintproject/flint-sundials;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-soslib = {
    url = github:flintproject/flint-soslib/aba1d97fda2cfa336fca260c47b13ed2c77d17b8;
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
          protobuf
          wxGTK32
        ];

        buildInputs = [
          protobuf
          boost177
          libmicrohttpd
          libxml2
          sqlite
          zeromq czmq
          wxGTK32
          flint-libsbml.packages.${system}.default
          flint-sundials.packages.${system}.default
          flint-soslib.packages.${system}.default
          clibsedml.packages.${system}.default
        ] ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
          Cocoa
        ]));

        src = ./source;

        configureFlags = [
          "--with-wxWidgets=${wxGTK32}"
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
          boost177
          wxGTK32
          libmicrohttpd
          libxml2
          sqlite
          zeromq czmq
          wxGTK32
          flint-libsbml.packages.${system}.default
          flint-sundials.packages.${system}.default
          flint-soslib.packages.${system}.default
          clibsedml.packages.${system}.default
        ] ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
          Cocoa
        ]));

        shellHook = ''
          export wxGTK32=${wxGTK32}
        '';

      };

    in {

      default = default-shell;

    });

  };
}
