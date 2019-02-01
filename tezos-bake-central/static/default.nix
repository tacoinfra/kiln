{pkgs ? (import ../.obelisk/impl {}).reflex-platform.nixpkgs} :
let
    semui = pkgs.callPackage ./semantic-ui.nix {};
in pkgs.stdenv.mkDerivation {
    name ="bakemonitor-staticFiles";
    src = ./.;
    builder = pkgs.writeScript "builder.sh" ''
      source "$stdenv/setup"
      mkdir -p $out
      cp -r $src/css $src/fonts $src/icons $src/images $out
      # this "should" be a symlink, but the static build doesn't handle
      # symlinks properly when generating filehashes.  copying it is slightly
      # wasteful, it takes extra space in the nix store.
      # ln -s ${semui.bakemonitor-semantic-ui} $out/semantic-ui
      cp -r ${semui.bakemonitor-semantic-ui} $out/semantic-ui
    '';
    passthru = { inherit semui; };
  }
