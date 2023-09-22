{ pkgs }:
with pkgs;
let
  outer-version = "v18.0-1";
  macos_version = "big_sur";
    tezos-baker-PtNairob = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtNairob-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0zffxh6nnfj8af9bi9qvrgh9m8bb44h4r79msrs3y21fa9c777n7";
    };
    tezos-baker-Proxford = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-Proxford-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "02kq1qg5si0x0k70rfzm9char9pp6dax8f8nq3iwzb95b70h5i0n";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1wzay0iq96bzjb0b4ff3ynm564rlvkxw1y34kqcpkwmybz54idjn";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1alspzvncw0y70sy0gcrn2jknjqbnr92r0s02bpmzlw63xx9vh7a";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtNairob}/${outer-version}/bin/tezos-baker-PtNairob $out/bin/tezos-baker-PtNairob
  chmod +x $out/bin/tezos-baker-PtNairob

  cp ${tezos-baker-Proxford}/${outer-version}/bin/tezos-baker-Proxford $out/bin/tezos-baker-Proxford
  chmod +x $out/bin/tezos-baker-Proxford

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
