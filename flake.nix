{
  description = "Prograrr - Download progress tracker for your *arr stack";

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
            echo "📊 Prograrr dev environment ready!"
            echo ""
            echo "Gleam $(gleam --version)"
            echo "Erlang/OTP $(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell)"
            echo ""
            echo "Commands:"
            echo "  gleam build    - Compile the project"
            echo "  gleam run      - Run the server"
            echo "  gleam test     - Run tests"
            echo ""
            echo "Required env vars: JELLYSEERR_API_KEY, SONARR_API_KEY, RADARR_API_KEY"
          '';
        };
      });
}
