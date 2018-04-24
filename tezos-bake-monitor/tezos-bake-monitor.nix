{ mkDerivation, stdenv, text, aeson, snap, lens, time, mtl, safe, async, optparse-applicative, http-client, http-client-tls, http-types, bytestring}:
mkDerivation {
  pname = "tezos-bake-monitor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ text aeson snap lens time mtl safe async optparse-applicative http-client http-client-tls http-types bytestring];
  description = "A wrapper for the Tezos baking client which monitors the output and provides statistics via a webserver";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
