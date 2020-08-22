{ nixpkgs ? import <nixpkgs> {}
, ghc ? nixpkgs.ghc
}:
nixpkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "infix-lisp";
  src = ./.;
}
