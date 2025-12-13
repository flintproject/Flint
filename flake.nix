{
  description = "A flake for building Flint";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-25.11;

  inputs.clibsedml = {
    url = github:flintproject/clibsedml/7f01bb301e3ae04f3b5eaa98cea3f61b7b93014c;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-libsbml = {
    url = github:flintproject/flint-libsbml/ade6791ce0fb9895c2b87521aa009785e6e7ad4d;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-sundials = {
    url = github:flintproject/flint-sundials/1a3763b61977134605d626d8468c0433c1c15e8a;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flint-soslib = {
    url = github:flintproject/flint-soslib/fb658bc93730cafa1a72b20e0d05ab2bbff47889;
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
          boost177
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
        ];

        src = ./source;

        configureFlags = [
          "--with-boost=${boost177.dev}"
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
        ];

        shellHook = ''
          export wxGTK32=${wxGTK32}
        '';

      };

    in {

      default = default-shell;

    });

  };
}
