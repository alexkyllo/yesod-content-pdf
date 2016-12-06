{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-builder, blaze-html, bytestring
      , conduit, data-default, directory, hspec, hspec-expectations
      , network-uri, process, stdenv, temporary, transformers
      , utf8-string, yesod-core
      }:
      mkDerivation {
        pname = "yesod-content-pdf";
        version = "0.2.0.2";
        src = ./.;
        isExecutable = true;
        libraryHaskellDepends = [
          base blaze-builder blaze-html bytestring conduit data-default
          directory network-uri process temporary transformers yesod-core
        ];
        testHaskellDepends = [
          base blaze-html hspec hspec-expectations utf8-string
        ];
        homepage = "https://github.com/alexkyllo/yesod-content-pdf#readme";
        description = "PDF Content Type for Yesod";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
