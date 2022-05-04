{ pkgs }:
with pkgs;
let
  outer-version = "v13.0-rc1-2";
  macos_version = "catalina";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1kngh4kvdkbwaqbc6vlr2jmzcgsvfz6jzh7alhbldkjfvc9jry7h";
    };
    tezos-baker-013-PtJakart = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-013-PtJakart-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "03fq074ff3gslsd56py7x1imq4l4wa04xkfmckggsxha8vwd8v4n";
    };
    tezos-baker-012-Psithaca = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-012-Psithaca-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "05ji7gw0x29ivxyhnrgkn5zzrvg1m2php4vdx7cbm5pgf29ccjds";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1aj901hbh64sxsxdgvf8hzmm0cgx2z1zl3wy6k5c9fb5zwak61ms";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0p45gdhhkl4j23q3yx5708pkab6gri75rlzym9zzng4p70knjz4z";
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

  cp ${tezos-baker-012-Psithaca}/${outer-version}/bin/tezos-baker-012-Psithaca $out/bin/tezos-baker-012-Psithaca
  chmod +x $out/bin/tezos-baker-012-Psithaca

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
