{
  description = "path-exclude";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    idris2-pkgs = {
      url = "github:claymager/idris2-pkgs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils";
        idris-server.follows = "";
      };
    };
  };

  outputs = { self, nixpkgs, utils, idris2-pkgs }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [ idris2-pkgs.overlay ];
          inherit system;
        };
      in
      {
        # TODO: package the actual program and look into using built-in
        # functions in the flake to simplify things
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.idris2.withLibs.contrib
            pkgs.idris2-pkgs.lsp.withLibs.contrib
            pkgs.clang-tools
          ];
        };
      });
}
