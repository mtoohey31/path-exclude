{
  description = "path-exclude";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: {
    overlays.default = final: prev: {
      path-exclude = final.stdenv.mkDerivation {
        pname = "path-exclude";
        version = "0.1.0";
        nativeBuildInputs = with final; [ gmp idris2 makeWrapper ];
        src = ./.;
        makeFlags = [ "PREFIX=$(out)" ];
        meta.platforms = final.idris2.meta.platforms;
        passthru.exePath = "/bin/px";
      };
    };
  } // utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    with import nixpkgs
      { overlays = [ self.overlays.default ]; inherit system; }; {
      apps.default = utils.lib.mkApp { drv = path-exclude; };

      packages.default = path-exclude;

      devShells.default = mkShell {
        packages = [ gmp idris2 ];
      };
    });
}
