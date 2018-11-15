{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "536b0a9243802347c299e077b5d85beb80d3a4a1";
      sha256 = "10wx0z5cd8dajr3rdskaq64v42ppa8dbb3rs3jyj872218xjz6nr";
    };

    notzero = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "notzero";
      rev = "04fbbe14773166de273577c0a6cb8dd89358fc78";
      sha256 = "0ypad68l7017my3vhcids21wx27lm381xx52c9q8pwviqlvdd077";
    };

    waargonaut = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "waargonaut";
      rev = "14c0928a0da5bbe50c1364e9a7ab40290b32ea0d";
      sha256 = "124ns0i71wiir9lybfqzpy6zqk4qxk3qa7wnhv04f8lr2f0vl5j9";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      notzero = sources.notzero;
      tagsoup-selection = pkgs.haskell.lib.doJailbreak super.tagsoup-selection;
      # waargonaut = sources.waargonaut;
    };
  };

  list-zipper = modifiedHaskellPackages.callPackage ./list-zipper.nix {};

in
  list-zipper
