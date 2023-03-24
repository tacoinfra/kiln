{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "16.1-1";

  src = fetchzip {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "sha256-l1D+4OmqN9tQnpMBp9vk4IfXhf9fHxGap3z0A63n72g=";
      stripRoot = false;
      };
  binaries = ["octez-client" "octez-node" "octez-baker-*" "octez-admin-client"];
  # Since 'tezos-*' binaries were renamed to 'octez-*' in v15.0 Octez release
  # but Kiln uses the old names, we rename them to 'tezos-*' while copying so
  # not to make Kiln source depend on binaries names update.
  installPhase = ''
  mkdir -p $out/bin
  for bin in $binaries ; do
    tezos_bin=$(echo $bin | sed "s/octez/tezos/")
    cp ${src}/$bin $out/bin/$tezos_bin
    chmod +x $out/bin/$tezos_bin
  done
  '';
  }
