{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, base, binary, bytestring, cereal
      , containers, data-default, directory, exceptions, fbida
      , filemanip-extra, filepath, generic-data, generic-lens, hslogger
      , HUnit, JuicyPixels, lens, lens-path, ListLike, mtl, network-uri
      , parsec, pretty, process, process-extras, pureMD5, QuickCheck
      , regex-compat-tdfa, regex-tdfa, safecopy, sr-errors, sr-extra
      , sr-log, stdenv, syb, template-haskell, temporary, text, th-lift
      , th-orphans, transformers, unexceptionalio-trans, unix, Unixutils
      , utf8-string, web-routes, web-routes-th
      }:
      mkDerivation {
        pname = "image-cache";
        version = "0.22.1";
        src = ./.;
        libraryHaskellDepends = [
          acid-state base binary bytestring cereal containers data-default
          directory exceptions filemanip-extra filepath generic-data
          generic-lens hslogger HUnit JuicyPixels lens lens-path ListLike mtl
          network-uri parsec pretty process process-extras pureMD5 QuickCheck
          regex-compat-tdfa regex-tdfa safecopy sr-errors sr-extra sr-log syb
          template-haskell temporary text th-lift th-orphans transformers
          unexceptionalio-trans unix Unixutils utf8-string web-routes
          web-routes-th
        ];
        libraryToolDepends = [ fbida ];
        homepage = "http://src.seereason.com/image-cache";
        description = "Support for image file processing and caching";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
