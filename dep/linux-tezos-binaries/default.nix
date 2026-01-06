{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "24.0-rc2";

  src = builtins.fetchTarball {
      # go to https://gitlab.com/tezos/tezos/-/releases/octez-v${version} and follow to "Static binaries"
      # package, find the link to 'octez-binaries-${version}-linux-x86_64.tar.gz' archive, and put it below
      # set sha256 to "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" to get the new hash from the error message
      # or use 'nix-prefetch-url <url>' to get it directly
      url = "https://gitlab.com/tezos/tezos/-/package_files/255491842/download";
      sha256 = "116dynw2fgfzb5n2kca2wd4zqzxzf53l7pndgqnxil2n4fvpxks1";
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
