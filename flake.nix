{
  description = "Media Request Tracker - Gleam + Lustre application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gleam
            erlang
            rebar3
            nodejs_20
            nodePackages.npm
          ];

          shellHook = ''
            echo "Gleam dev environment ready!"
            echo "Gleam version: $(gleam --version)"
          '';
        };
      });
}
