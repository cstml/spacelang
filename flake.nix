{
  description = "Toy Language";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs@{self,nixpkgs,flake-utils}:
    flake-utils.lib.eachDefaultSystem (system :
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ncurses = pkgs.ncurses;

        compiler = pkgs.stdenv.mkDerivation {
          name = "compiler";
          src = self;
          buildInputs = with pkgs;[
            sbcl
            gnumake
            lispPackages.quicklisp
            ncurses
          ];

          shellHook = ''
            export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${ncurses}/lib;
          '';
        };
      in
        {
          defaultPackage = compiler;
        }
    );
}
