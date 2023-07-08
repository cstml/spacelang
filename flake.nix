{
  description = "Toy Language";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs@{self,nixpkgs,flake-utils}:
    flake-utils.lib.eachDefaultSystem (system :
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        compiler = pkgs.stdenv.mkDerivation {
          name = "compiler";
          src = self;
          buildInputs = with pkgs;[
            gnumake
          ];
        };
      in
        {
          defaultPackage = compiler;


        }
    );
}
