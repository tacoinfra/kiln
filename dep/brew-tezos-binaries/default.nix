{ pkgs }:
with pkgs;
let
  outer-version = "v17.0-1";
  macos_version = "big_sur";
    tezos-baker-PtNairob = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtNairob-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "12kgn6jybbj48ilry2h9gf31aa1rym7ah6hhayisrvix9jg9zg08";
    };
    tezos-baker-PtMumbai = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtMumbai-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0qadr84kcm4y6ivqzlffyvmizji78a20y0bz17bfnll9kjy7ni83";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1hhng2q2b62q83833j197jqazzprmvadmalxjfxdcd3m8pyzg96a";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "02868qspn1yckjmzxa3il4iqnyc32k98id9pm42ajpkn9ngf2dv9";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtNairob}/${outer-version}/bin/tezos-baker-PtNairob $out/bin/tezos-baker-PtNairob
  chmod +x $out/bin/tezos-baker-PtNairob

  cp ${tezos-baker-PtMumbai}/${outer-version}/bin/tezos-baker-PtMumbai $out/bin/tezos-baker-PtMumbai
  chmod +x $out/bin/tezos-baker-PtMumbai

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
