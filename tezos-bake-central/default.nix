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
  nixpkgs1909 = import dep/nixpkgs1909 {};
in
obelisk.project ./. ({ pkgs, ... }@args:
  let
    inherit (obelisk.reflex-platform) hackGet;

    rhyolite = obelisk;
    nodeKit = if tezosScopedKit != null
            then tezosScopedKit
            else import ../dep/platform-specific-binaries.nix { inherit system pkgs;};

    hsOnly = attrs: pkg: pkg.overrideAttrs ({ src, ... }: {
      src = pkgs.lib.cleanSourceWith {
        filter = (name: type: type == "directory" || (!(pkgs.lib.hasSuffix ".hi" name) && !(pkgs.lib.hasSuffix ".o" name)));
        src = pkgs.lib.cleanSource src;
      };
    } // attrs);

    frontendOnly =
        let attrs = {
            postInstall  = ''
             ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${pkgs.nodejs-slim} $out/bin/frontend
             '';
                };
        in pkg: hsOnly attrs pkg;

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
      # functor-infix = hackGet dep/functor-infix;
      jsaddle-dom = hackGet dep/jsaddle-dom;
      micro-ecc = hackGet dep/micro-ecc-haskell;
      named = hackGet dep/named; # TODO: Drop once package set includes 0.3.0.0
      reflex-dom-forms = hackGet dep/reflex-dom-forms;
      semantic-reflex = hackGet dep/semantic-reflex + "/semantic-reflex";
      tezos-bake-monitor-lib = hackGet dep/tezos-bake-monitor-lib + "/tezos-bake-monitor-lib";
      tezos-noderpc = hackGet dep/tezos-bake-monitor-lib + "/tezos-noderpc";
    };

    overrides =
     let appOverlay = self: super: with pkgs.haskell.lib; {
          common = haddock-build (checkHlint (hsOnly {} (if distMethod == null
            then super.common
            else enableCabalFlag super.common distMethod)));
          backend = haddock-build (checkHlint (hsOnly {} (overrideCabal super.backend (drv:{
            librarySystemDepends = drv.librarySystemDepends or [] ++ [nodeKit];
          }))));
          base58-bytestring = dontCheck super.base58-bytestring; # disable tests for GHCJS build
          email-validate = dontCheck super.email-validate; # disable tests for GHCJS build
          extra = dontCheck super.extra; # disable unreliable tests (https://github.com/ndmitchell/extra/issues/37)
          lens-aeson = dontCheck super.lens-aeson;
          frontend = haddock-build (checkHlint (frontendOnly super.frontend));
          markdown-unlit = pkgs.haskell.lib.dontCheck super.markdown-unlit;
          memory = dontCheck (self.callHackage "memory" "0.14.17" {});
          reflex-dom-core = dontCheck super.reflex-dom-core;
          semantic-reflex = dontHaddock (dontCheck super.semantic-reflex);
          silently = pkgs.haskell.lib.dontCheck super.silently;
          terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};
          tezos-bake-monitor-lib = test-runner (haddock-build super.tezos-bake-monitor-lib);
          tezos-noderpc = checkHlint (haddock-build super.tezos-noderpc);
          };

        postgresql-override = pkgs.postgresql.overrideAttrs (oldAttrs:
            let libxml2-noPythonSupport = pkgs.libxml2.override { pythonSupport = false;};
            in { buildInputs = builtins.filter (x: ! (pkgs.lib.hasPrefix "libxml2" x.name)) oldAttrs.buildInputs ++ [libxml2-noPythonSupport]; }
            );

        # This explicit dependency may be taken out soon if the changes are accepted by Obsidian.
        gargoyleSrc = pkgs.fetchFromGitHub {
          owner = "obsidiansystems";
          repo = "gargoyle";
          rev = "941ca7a25403bab4c719e669db36dc18b240b996";
          sha256 = "18vvvp29ph112myxqmw4cgf1x6q0xs6jwdmg4k9fghcpav8acjd7";};
        gargoyleOverlay = import gargoyleSrc { postgresql = postgresql-override;};


        # For upgrading to later GHC versions.
        # ghcOverlay = self: super: assert (builtins.hasAttr "haskell" pkgs); builtins.trace (pkgs.haskell.packages.ghc864.ghc.version) {};
        # ghcOverlay = self: super: {ghc = pkgs.haskell.packages.ghc864.ghc;};

        baseOverlay = self: super:
            let callHackageDirect = {pkg,ver,sha256}:
                let pkgver = "${pkg}-${ver}";
                in self.callCabal2nix pkg (pkgs.fetchzip {
                   url = "mirror://hackage/${pkgver}.tar.gz";
                   inherit sha256;
                   });
            in { which = callHackageDirect { pkg = "which"; ver = "0.1.0.0"; sha256 = "1c8svdiv378ps63lwn3aw7rv5wamlpmzgcn21r2pap4sx7p08892";} {};};
     in with pkgs.lib; foldr composeExtensions baseOverlay [ rhyolite.haskellOverrides appOverlay gargoyleOverlay ];
  }) // {
    dev.extraGhciArgs = ["-fobject-code"];
  }
