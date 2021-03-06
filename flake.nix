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
    utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            overlays = [ idris2-pkgs.overlay ];
            inherit system;
          };
          inherit (pkgs.idris2-pkgs._builders) idrisPackage devEnv;
        in
        rec {
          apps.default = utils.lib.mkApp {
            drv = packages.default;
            name = "px";
          };

          packages.default = idrisPackage ./. {
            buildPhase = "make";
          };

          devShells.default = pkgs.mkShell {
            packages = [
              (devEnv self.packages.${system}.default)
              pkgs.clang-tools
            ];
          };
        }) // {
      overlays.default = (final: _: {
        path-exclude = self.packages.${final.system}.default;
      });
    };
}
