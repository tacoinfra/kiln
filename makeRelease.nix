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
        Depends: util-linux
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
          mkdir -p $DEBDIR/etc/${pkgName}
          mkdir -p $DEBDIR/lib/systemd/system/

          mkdir -p $DEBDIR/${root-dir}/{nix,dev,proc,sys,etc,run,usr,var,bin,lib,lib64,tmp}
          mkdir -p $DEBDIR/${exe-dir}


          ln -s ${obApp.exe}/* $DEBDIR/${exe-dir}/
          cp ${control} $DEBDIR/DEBIAN/control
          cp ${deb-copyright} $DEBDIR/DEBIAN/copyright
          cp $src/CHANGELOG.md $DEBDIR/DEBIAN/changelog
          cp ${deb-pre-install}  $DEBDIR/DEBIAN/preinst
          cp ${deb-post-install}  $DEBDIR/DEBIAN/postinst
          cp ${deb-pre-rm}  $DEBDIR/DEBIAN/prerm
          cp ${deb-post-rm}  $DEBDIR/DEBIAN/postrm

          cp ${run-kiln-exe}/bin/run-kiln $DEBDIR/usr/bin/
          sed -i '1s;^;#!/bin/bash\n;' $DEBDIR/usr/bin/run-kiln

          cp ${serviceFiles} $DEBDIR/lib/systemd/system/${pkgName}.service
          echo "KILNARGS=" > $DEBDIR/etc/${pkgName}/args

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

  deb-pre-install = pkgs.writeTextFile { name = "${pkgName}.preinst"; executable = true; text = ''
    #!/bin/sh
    set -e
    # nothing here

    exit 0
  ''; };

  deb-post-install = pkgs.writeTextFile { name = "${pkgName}.postinst"; executable = true; text = ''
    #!/bin/sh
    set -e

    case $1 in
        configure)
         addgroup --system --quiet kiln
         adduser --system --quiet --ingroup kiln --no-create-home --home /var/lib/kiln kiln
         chown -R kiln /var/lib/kiln
    esac
    if [ -d /run/systemd/system ]; then
        systemctl --system daemon-reload >/dev/null || true
        deb-systemd-invoke start kiln.service >/dev/null
    fi
    exit 0
  ''; };

  deb-pre-rm = pkgs.writeTextFile { name = "${pkgName}.prerm"; executable = true; text = ''
    #!/bin/sh
    set -e
    if [ -d /run/systemd/system ]; then
        deb-systemd-invoke stop kiln.service >/dev/null
    fi
    if [ "$1" = remove ]; then
         # delete only the data-dir, leave db intact
         rm -rf ${data-dir}/*
    fi # else it could be 'upgrade', 'failed-upgrade'

    exit 0
  ''; };

  deb-post-rm = pkgs.writeTextFile { name = "${pkgName}.postrm"; executable = true; text = ''
    #!/bin/sh
    set -e
    if [ -d /run/systemd/system ]; then
        systemctl --system daemon-reload >/dev/null || true
    fi
    case $1 in
        purge)
        deluser --system --quiet kiln || true
        delgroup --system --quiet kiln || true
        rm -rf /var/lib/kiln
    esac

    exit 0
  ''; };

  deb-copyright = pkgs.writeTextFile { name = "${pkgName}-deb-copyright"; text = ''
    Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
    Upstream-Name: Kiln
    Source: https://gitlab.com/obsidian.systems/tezos-bake-monitor

    Files: *
    Copyright: 2019 obsidian.systems
    License: MIT
      Permission is hereby granted, free of charge, to any person obtaining a copy
      of this software and associated documentation files (the "Software"), to deal
      in the Software without restriction, including without limitation the rights
      to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      copies of the Software, and to permit persons to whom the Software is
      furnished to do so, subject to the following conditions:

      The above copyright notice and this permission notice shall be included in all
      copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      SOFTWARE.
  ''; };

  run-kiln-exe =
    let
      # Not using writeScriptBin here, as we want to use /bin/bash
      run-backend = pkgs.writeTextFile { name = "run-backend"; executable = true; text = ''
        #!/bin/bash
        cd ${exe-dir}
        ./backend --kiln-data-dir=${data-dir} $@
      ''; };

      # Since gargoyle (or rather postgresql) can only work if invoked by a non-root user
      # We need to do a nested unshare (after doing mount) to change to a non-root shell
      # But the second 'unshare --user' (to go from root -> nobody) cannot happen after doing chroot
      # (see error EPERM, in man 2 unshare)
      # 
      # So in order to do a nested unshare we instead do 'pivot_root'
      do-mount-and-pivot = pkgs.writeTextFile { name = "do-mount-and-pivot"; executable = true; text = ''
        #!/bin/bash
        mount --bind ${root-dir}  ${root-dir}
        mount --rbind /proc  ${root-dir}/proc
        mount --rbind --make-unbindable ${nix-store-root}/nix   ${root-dir}/nix
        mount --rbind --make-unbindable /dev   ${root-dir}/dev
        mount --rbind --make-unbindable /sys   ${root-dir}/sys
        mount --rbind --make-unbindable /etc   ${root-dir}/etc
        mount --rbind --make-unbindable /run   ${root-dir}/run
        mount --rbind --make-unbindable /usr   ${root-dir}/usr
        mount --rbind --make-unbindable /var   ${root-dir}/var
        mount --rbind --make-unbindable /bin   ${root-dir}/bin
        mount --rbind --make-unbindable /lib   ${root-dir}/lib
        mount --rbind --make-unbindable /lib64 ${root-dir}/lib64
        mount --rbind --make-unbindable /tmp   ${root-dir}/tmp
        mkdir -p ${root-dir}/oldroot
        cd ${root-dir}
        pivot_root . oldroot
        cd /
        exec ${do-umount-and-unshare} $@
      ''; };

      do-umount-and-unshare = pkgs.writeTextFile { name = "do-umount-and-unshare"; executable = true; text = ''
        #!/bin/bash
        umount -l oldroot
        exec unshare --user ${run-backend} $@
      ''; };

      # not putting #!/bin/bash here as it gets replaced by /nix/store during nix-build
      mainScript = pkgs.writeTextFile { name = "run-kiln-mainScript"; executable = true; text = ''
        exec unshare --mount --map-root-user ${nix-store-root}/${do-mount-and-pivot} $@
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

    [Service]
    Type=simple
    EnvironmentFile=/etc/${pkgName}/args
    ExecStart=/usr/bin/run-kiln $KILNARGS
    Restart=always
    User=kiln

    [Install]
    WantedBy=multi-user.target
  ''; };
in {
  inherit kiln-debian;
}