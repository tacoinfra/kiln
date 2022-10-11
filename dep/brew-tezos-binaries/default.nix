{ pkgs }:
with pkgs;
let
  outer-version = "v14.1-1";
  macos_version = "big_sur";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1zn4glvwwaq2b17wkzwcw4qkakdgfi7gqjacjnd85i4mkbd79ia1";
    };
    tezos-baker-013-PtJakart = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-013-PtJakart-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0zjgajhq657bqlsp5qsnxxnhd91yi42pvi1c27marag81s9lw1ir";
    };
    tezos-baker-014-PtKathma = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-014-PtKathma-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1vrrwlqhglhfhgwm8ninraq5jxh8zanq5bqay4f93s146l7pcihd";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0xy1kawx4f1h70wab6hlfxkw9j8f0542fr81p02d1dxdw56mcg66";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1kkjasrk507zlw9dk2l14a0l44lgilgqwbbwqhdv4vfwm8jccla3";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-013-PtJakart}/${outer-version}/bin/tezos-baker-013-PtJakart $out/bin/tezos-baker-013-PtJakart
  chmod +x $out/bin/tezos-baker-013-PtJakart

  cp ${tezos-baker-014-PtKathma}/${outer-version}/bin/tezos-baker-014-PtKathma $out/bin/tezos-baker-014-PtKathma
  chmod +x $out/bin/tezos-baker-014-PtKathma

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
