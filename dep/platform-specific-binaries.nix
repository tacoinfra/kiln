{ system ? builtins.currentSystem }:

let system-binaries = {
                x86_64-linux = import ./serokell-tezos-binaries;
                x86_64-darwin = import ./brew-tezos-binaries {};
                };

in if builtins.hasAttr system system-binaries
   then builtins.getAttr system system-binaries
   else builtins.abort "This is system ${system} is unsupported."
