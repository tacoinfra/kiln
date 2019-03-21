#  binarypackage.service -x3

#     If this exists, it is installed into lib/systemd/system/binarypackage.service in binarypackage.

#     See dh_systemd_enable(1), dh_systemd_start(1), and dh_installinit(1).
   
#  binarypackage.manpages -x2

#     List man pages to be installed.

#     See dh_installman(1).

# debian/changelog

# debian/copyright
{ pkgs
, obApp
, pkgName
, version
}:
let
  maintainer = "Obsidian Systems <tezos@obsidian.systems>";
  description = "Kiln, provides individuals running Tezos nodes and bakers with a locally hosted graphical interface enabling easy and effective monitoring.";


  var-prefix = "/var/lib/${pkgName}";

  # Here the chroot will be done
  root-dir = "${var-prefix}/root-dir";

  # This will have the links to "backend, frontend.assets, etc"
  exe-dir = "${var-prefix}/exe-dir";

  # This is kiln-data-dir
  data-dir = "${var-prefix}/data-dir";

  # Path where "/nix" is copied
  nix-store-root = "/usr/share/${pkgName}";

  kiln-debian =
    let
      control = pkgs.writeTextFile { name = "control"; text = ''
        Package: ${pkgName}
        Version: ${version}
        Architecture: amd64
        Maintainer: ${maintainer}
        Depends: 
        Description: ${description}
      ''; };

    in pkgs.stdenv.mkDerivation {
        name = "${pkgName}-${version}-debian-pkg";
        src = ./.;
        buildInputs = [ pkgs.dpkg pkgs.perl ];
        exportReferencesGraph =
          [ "closure" run-kiln-exe ];
        builder = pkgs.writeScript "builder.sh" ''
          source "$stdenv/setup"
          mkdir -p $out

          export DEBDIR=$TMPDIR/${pkgName}_${version}-1

          # make debian file structure
          mkdir -p $DEBDIR/DEBIAN
          mkdir -p $DEBDIR/usr/bin
          mkdir -p $DEBDIR/lib/systemd/system/

          mkdir -p $DEBDIR/${root-dir}/{nix,dev,proc,sys,etc,run,usr,var,bin,lib,lib64,tmp}
          mkdir -p $DEBDIR/${exe-dir}


          ln -s ${obApp.exe}/* $DEBDIR/${exe-dir}/
          cp ${control} $DEBDIR/DEBIAN/control
          cp ${run-kiln-exe}/bin/run-kiln $DEBDIR/usr/bin/
          sed -i '1s;^;#!/bin/bash\n;' $DEBDIR/usr/bin/run-kiln
          cp ${serviceFiles} $DEBDIR/lib/systemd/system/${pkgName}.service

          # copy nix closure
          storePaths=$(perl ${pkgs.pathsFromGraph} closure)
          mkdir -p $DEBDIR/${nix-store-root}/nix/store
          cp -prd $storePaths $DEBDIR/${nix-store-root}/nix/store/

          # mkdir -p $DEBDIR/lib/systemd/system/${pkgName}.service
          # chown root:root -R $DEBDIR/*
          chmod 0755 $DEBDIR/usr/bin/*

          ${pkgs.dpkg}/bin/dpkg-deb --build $DEBDIR $out
        '';
    };

  run-kiln-exe =
    let
      # Not using writeScriptBin here, as we want to use /bin/bash
      run-backend = pkgs.writeTextFile { name = "run-backend"; executable = true; text = ''
        #!/bin/bash
        cd ${exe-dir}
        ./backend --kiln-data-dir=${data-dir} $@
      ''; };

      do-mount = pkgs.writeTextFile { name = "do-mount"; executable = true; text = ''
        #!/bin/bash
        mount --rbind --make-unbindable ${nix-store-root}/nix   ${root-dir}/nix
        mount --rbind --make-unbindable /dev   ${root-dir}/dev
        mount --rbind --make-unbindable /proc  ${root-dir}/proc
        mount --rbind --make-unbindable /sys   ${root-dir}/sys
        mount --rbind --make-unbindable /etc   ${root-dir}/etc
        mount --rbind --make-unbindable /run   ${root-dir}/run
        mount --rbind --make-unbindable /usr   ${root-dir}/usr
        mount --rbind --make-unbindable /var   ${root-dir}/var
        mount --rbind --make-unbindable /bin   ${root-dir}/bin
        mount --rbind --make-unbindable /lib   ${root-dir}/lib
        mount --rbind --make-unbindable /lib64 ${root-dir}/lib64
        mount --rbind --make-unbindable /tmp   ${root-dir}/tmp
        exec chroot ${root-dir} ${nix-store-root}/${run-backend} $@
      ''; };

      # not putting #!/bin/bash here as it gets replaced by /nix/store during nix-build
      mainScript = pkgs.writeTextFile { name = "run-kiln-mainScript"; executable = true; text = ''
        exec unshare --mount --map-root-user --user ${nix-store-root}/${do-mount} $@
      ''; };

    in pkgs.stdenv.mkDerivation {
      name = "run-kiln-exe";
      src = ./.;
      propagatedBuildInputs = [ obApp.exe ];
      installPhase = ''
        mkdir -p $prefix/bin
        cp ${mainScript} $prefix/bin/run-kiln
      '';
    };


  serviceFiles = pkgs.writeTextFile { name = "${pkgName}.service"; text = ''
    [Unit]
    Description=Kiln
    After=postgres.service

    [Service]
    Type=simple
    ExecStart=/usr/bin/run-kiln
    Restart=always

    [Install]
    WantedBy=multi-user.target
  ''; };
in {
  inherit kiln-debian;
}