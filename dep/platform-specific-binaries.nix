{ system ? builtins.currentSystem, pkgs, source }:

let system-binaries = {
                x86_64-linux =  if source
                    then (import ./build-serokell-tezos-binaries).binaries
                    else pkgs.callPackage ./serokell-tezos-binaries {};
                x86_64-darwin = pkgs.callPackage ./brew-tezos-binaries {};
                };

in if builtins.hasAttr system system-binaries
   then  builtins.getAttr system system-binaries
   else builtins.abort "This system ${system} is unsupported."
