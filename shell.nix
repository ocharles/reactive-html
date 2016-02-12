let nixpkgs = import <nixpkgs> {};
    reflex-platform = nixpkgs.fetchgit {
      url = "git://github.com/reflex-frp/reflex-platform.git";
      sha256 = "c33e4d4c3661181234a35081b482ef26c3954fd4745cff8389b6845872529400";
      rev = "abc2bc56d64e63fb8438172e84eb4dd125a2a684";
    };
    reactive-banana-src = nixpkgs.fetchgit {
      url = git://github.com/ocharles/reactive-banana;
      rev = "1146dac430062f015d6978b25211c58f2df17c39";
      sha256 = "be37ffe461f672c9642a62734038e1ecc5a5e5d860604255fab7a1f65a83e099";};
    pkgs = import reflex-platform {};
    haskell = pkgs.ghcjs.override {
      overrides = self: super: {
        # Fork of reactive-banana with no garbage collection
        reactive-banana = nixpkgs.haskell.lib.overrideCabal super.reactive-banana
          (drv: { src = "${reactive-banana-src}/reactive-banana"; });
      };
    };
in nixpkgs.stdenv.mkDerivation {
     name = "reactive-html";
     buildInputs = [ (haskell.ghcWithPackages (hs: [ hs.reactive-banana hs.ghcjs-dom hs.patches-vector ])) ];
   }
