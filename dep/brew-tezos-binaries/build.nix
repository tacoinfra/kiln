# run this file with "nix-build build.nix"
with import <nixpkgs> {};
callPackage ./. {}
