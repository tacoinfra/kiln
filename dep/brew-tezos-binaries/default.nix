########################################
# THIS IS NOT EVEN CLOSE TO BEING DONE #
########################################
{ stdenv }:
    # this is just a temporary hack
    # PLEASE MAKE SURE THAT THIS FILE EXISTS (brewDirectory)
    let brewDirectory = "/usr/local/Cellar/tezos/7.2/bin/";
    in if builtins.pathExists brewDirectory
       then stdenv.mkDerivation rec {
              name = "tezos-binaries-${version}";
              version = "7.2-1";

              src = builtins.fetchurl {url = "file://" + brewDirectory; sha256 = "0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73";};

              installPhase = ''
              mkdir -p $out/bin
              for bin in $(ls ${src}) ; do
                cp ${src}/$bin $out/bin/multinetwork-$bin
              done
              chmod +x $out/bin
              '';

                 }
       else builtins.abort "brew-tezos-binaries.nix: Cannot find tezos-binaries"


              # for bin in $(ls $out/bin) ; do
                # mv $out/bin/$bin $out/bin/multinetwork-$bin
              # done

              # for bin in $(ls ${src}) ; do
              #   cp ${src}/$bin $out/bin/multinetwork-$bin
              #   chmod +x $out/bin/multinetwork-$bin
              # done
##############################################
# THIS IS NOT EVEN BEING CLOSE TO BEING DONE #
##############################################
