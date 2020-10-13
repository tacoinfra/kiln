{ system ? builtins.currentSystem
, supportGargoyle ? true  # This must default to `true` for 'ob run' to work.
, profiling ? false
, distMethod ? null
, tezosScopedKit ? null
, runTests ? false
, buildHaddock ? false
, closure-compiler-setting ? "SIMPLE" # set this to null to skip closure-compiler step
}:
let
  obelisk = import .obelisk/impl { inherit system profiling; };
in
obelisk.project ./. ({ pkgs, ... }@args:
  let
    inherit (obelisk.reflex-platform) hackGet;
    rhyolite = obelisk;
    nodeKit = if tezosScopedKit != null
            then tezosScopedKit
            else import ../dep/platform-specific-binaries.nix { inherit system pkgs;};

    hsOnly = super: component: pkg: pkg.overrideAttrs ({ src, ... }: {
      src = pkgs.lib.cleanSourceWith {
        filter = (name: type: type == "directory" || (!(pkgs.lib.hasSuffix ".hi" name) && !(pkgs.lib.hasSuffix ".o" name)));
        src = pkgs.lib.cleanSource src;
      };
     disallowedReferences = [ super.tezos-bake-monitor-lib ];
     postInstall = if component != null then ''
     ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${super.tezos-bake-monitor-lib} $out/bin/${component}
     ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${super.gargoyle-postgresql-nix} $out/bin/${component}
     ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${pkgs.gmp} $out/bin/${component}
     ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${pkgs.postgresql} $out/bin/${component}
       '' else "";
    });

    checkHlint = pkg: pkg.overrideAttrs ({ preConfigure ? "", src, ... }: {
      preConfigure = ''
        (
          echo "Checking for lint"

          set -x
          '${pkgs.hlint}/bin/hlint' --version
          '${pkgs.hlint}/bin/hlint' --hint '${builtins.path { path = ../.hlint.yaml; name = "hlint.yaml"; }}' '${src}' || exit 1
        )
        ${preConfigure}
      '';
    });
    haddock-build = if buildHaddock then pkgs.haskell.lib.doHaddock else pkgs.haskell.lib.dontHaddock;
    test-runner = if runTests then pkgs.haskell.lib.doCheck else pkgs.haskell.lib.dontCheck;
  in {
    staticFiles = pkgs.callPackage ./static { pkgs = obelisk.nixpkgs; };
    # staticFilesImpure = toString ./result-static;
    __closureCompilerOptimizationLevel = closure-compiler-setting;
    packages = {
      # Obelisk thunks. Place here so can repl and build locally when unpacked.
      functor-infix = hackGet dep/functor-infix;
      jsaddle-dom = hackGet dep/jsaddle-dom;
      micro-ecc = hackGet dep/micro-ecc-haskell;
      named = hackGet dep/named; # TODO: Drop once package set includes 0.3.0.0
      reflex-dom-forms = hackGet dep/reflex-dom-forms;
      semantic-reflex = hackGet dep/semantic-reflex + "/semantic-reflex";
      tezos-bake-monitor-lib = hackGet dep/tezos-bake-monitor-lib + "/tezos-bake-monitor-lib";
      tezos-noderpc = hackGet dep/tezos-bake-monitor-lib + "/tezos-noderpc";
    };

    overrides = pkgs.lib.composeExtensions rhyolite.haskellOverrides (self: super: with pkgs.haskell.lib; {
      common = haddock-build (checkHlint (hsOnly super null (if distMethod == null
        then super.common
        else enableCabalFlag super.common distMethod)));
      backend = haddock-build (checkHlint (hsOnly super "backend" (overrideCabal super.backend (drv:{
        librarySystemDepends = drv.librarySystemDepends or [] ++ [nodeKit];
        postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
      }))));
      base58-bytestring = dontCheck super.base58-bytestring; # disable tests for GHCJS build
      email-validate = dontCheck super.email-validate; # disable tests for GHCJS build
      extra = dontCheck super.extra; # disable unreliable tests (https://github.com/ndmitchell/extra/issues/37)
      lens-aeson = dontCheck super.lens-aeson;
      frontend = haddock-build (checkHlint (hsOnly super null super.frontend));
      markdown-unlit = pkgs.haskell.lib.dontCheck super.markdown-unlit;
      memory = dontCheck (self.callHackage "memory" "0.14.17" {});
      semantic-reflex = dontHaddock (dontCheck super.semantic-reflex);
      silently = pkgs.haskell.lib.dontCheck super.silently;
      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};
      tezos-bake-monitor-lib = test-runner (haddock-build super.tezos-bake-monitor-lib);
      tezos-noderpc = checkHlint (haddock-build super.tezos-noderpc);
    });
  }) // {
    dev.extraGhciArgs = ["-fobject-code"];
  }
