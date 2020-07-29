{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.3-1";

  src = builtins.fetchTarball {
      # Put this line back in once the official release is out! (The sha256 hash may change too)
      # url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      url = "https://github.com/serokell/tezos-packaging/releases/download/auto-release/binaries-${version}.tar.gz";
      sha256 = "0q9yb1krq2a0fb7lpap4ga34dxr81hc6gn6vyddd52xplqyxjwdl";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
