{ system ? builtins.currentSystem, pkgs }:

let system-binaries = {
                # x86_64-linux =  pkgs.callPackage ./serokell-tezos-binaries {};
                x86_64-linux =  (import ./build-serokell-tezos-binaries).binaries;
                x86_64-darwin = pkgs.callPackage ./brew-tezos-binaries {};
                };

in if builtins.hasAttr system system-binaries
   then  builtins.getAttr system system-binaries
   else builtins.abort "This system ${system} is unsupported."
