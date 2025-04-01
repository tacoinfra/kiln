{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "22.0-1";

  src = fetchzip {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-octez-v${version}.tar.gz";
      sha256 = "sha256-jmt03fSzMdc31Z9qR0/9R5v+NR0qnZG7rw/TAzZW1Z8=";
      stripRoot = false;
      };
  binaries = ["octez-client" "octez-node" "octez-baker-*"];
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
