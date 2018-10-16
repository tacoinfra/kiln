{pkgs ? (import ../.obelisk/impl {}).reflex-platform.nixpkgs} :
let
    semui = pkgs.callPackage ./semantic-ui.nix {};
in pkgs.stdenv.mkDerivation {
    name ="bakemonitor-staticFiles";
    src = ./.;
    builder = pkgs.writeScript "builder.sh" ''
      source "$stdenv/setup"
      mkdir -p $out
      cp -r $src/css $src/fonts $src/icons $src/logo.svg $out
      ln -s ${semui.bakemonitor-semantic-ui} $out/semantic-ui
    '';
  }
