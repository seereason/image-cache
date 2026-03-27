{
  outputs = { ... }: {};
}
# {
#   description = "lens-path";

#   inputs = {
#     pkgsets.url   = "git+ssh://git@github.com/cliffordbeshers/cbgit?dir=nix-config/sr-flake/sr-nixpkgs&ref=main";
#     sr-libs.url   = "git+ssh://git@github.com/cliffordbeshers/cbgit?dir=nix-config/sr-flake/sr-libs&ref=main";
#     flake-utils.url = "github:numtide/flake-utils";
#   };

#   outputs = { self, pkgsets, sr-libs, flake-utils }:
#     flake-utils.lib.eachDefaultSystem (system:
#       let
#         pkgs        = pkgsets.lib.${system}.pkgs;
#         haskellPkgs = pkgsets.lib.${system}.ghcPackages.extend (sr-libs.overlay pkgs);

#         lens-path = haskellPkgs.callCabal2nix "lens-path" ./. {};
#       in {
#         packages.default = lens-path;

#         devShells.default = haskellPkgs.shellFor {
#           packages    = p: [ p.lens-path ];
#           buildInputs = [
#             haskellPkgs.cabal-install
#             haskellPkgs.haskell-language-server
#             haskellPkgs.hlint
#             pkgs.cabal2nix
#           ];
#           withHoogle = true;
#         };
#       });
# }
