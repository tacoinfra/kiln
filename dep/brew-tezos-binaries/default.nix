{ pkgs }:
with pkgs;
let
  outer-version = "v10.3-1";
  macos_version = "mojave";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0xbyv28p9rrg3dw9plx1m2n5cb0bl3klsfz8xwh1av35814q6m0z";
    };
    tezos-baker-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0ck26zz0gp1n03nwmsjy3qwsfqm5bmr2pi1i3mw1w8f6nl5gf184";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0h5wgv2k61ra4rbp2nq9da639zfminrgjgr5x4k8hbkii4wls7z8";
    };
    tezos-endorser-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0lp7inq1hf9imvmdj8r0ryqjsaqmincxccw2bqpmmh6qd9m577sp";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "01hrd5rf3imavsa8ps2i02v9npq185fsa5kx1v1yjwnxzfpwawkz";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-010-PtGRANAD}/${outer-version}/bin/tezos-baker-010-PtGRANAD $out/bin/tezos-baker-010-PtGRANAD
  chmod +x $out/bin/tezos-baker-010-PtGRANAD

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-010-PtGRANAD}/${outer-version}/bin/tezos-endorser-010-PtGRANAD $out/bin/tezos-endorser-010-PtGRANAD
  chmod +x $out/bin/tezos-endorser-010-PtGRANAD

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
