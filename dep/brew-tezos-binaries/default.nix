{ pkgs }:
with pkgs;
let
  outer-version = "v13.0-1";
  macos_version = "catalina";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "187pcwah69lgpq0ljjcfhs1phnfdndg7l991h9w8na1q84bj3vbc";
    };
    tezos-baker-013-PtJakart = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-013-PtJakart-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0b9gj7qfvhcqi2ca56kz979q34iy7k1ls13m5zzh8fjqdq907lqw";
    };
    tezos-baker-012-Psithaca = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-012-Psithaca-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0mw39i2jrcmdr3fqzr86hy7mmc3y9s96sqi52xh2jvvmzs9d07ip";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1rjwimk15v8fgqr9dnb7r64k03rac48sdaaamrv1r115lndwaphm";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1s2jnfxwfy2a07sqap7k8fwl81akh1gmwf0fiqwshbp6hb9xlmjn";
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
