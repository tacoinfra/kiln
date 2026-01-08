{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "24.0-rc2";

  src = builtins.fetchTarball {
    url = "https://octez.tezos.com/releases/octez-v${version}/binaries/x86_64/octez-v${version}.tar.gz";
    sha256 = "13xmkhi0s9zrx7pbn1lalzhc96ykaq3n0r8gi6cislmdwgyfv3l4";
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
