{ pkgs
, obApp
, nodeKit
, pkgName
, version
}:
let
  maintainer = "Obsidian Systems <tezos@obsidian.systems>";
  description = "Application for participating in the Tezos blockchain.\n  Kiln is an application for participating in the Tezos blockchain. It allows the user to run a node, produce and validate blocks, and vote on protocol amendments through its interface. It can also be used to monitor existing Tezos infrastructure, and sends notifications via Telegram or SMTP when It detects an issue.";

  var-prefix = "/var/lib/${pkgName}";

  # Here the pivot_root will be done
  root-dir = "${var-prefix}/root-dir";

  # This will have the links to "backend, frontend.assets, etc" and "db"
  # It will be preserved even after uninstall, but purge will remove this
  exe-dir = "${var-prefix}/exe-dir";

  # This is '--kiln-data-dir'
  # This will be cleared when kiln is removed/uninstalled (but preserved when upgrading)
  data-dir = "${var-prefix}/data-dir";

  # Path where "/nix" closure is copied
  nix-store-root = "/usr/share/${pkgName}";

  kiln-debian =
    let
      # util-linux is required for pivot_root
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
        src = ./CHANGELOG.md;
        exportReferencesGraph = [ "closure" run-kiln-exe ];

        builder = pkgs.writeScript "builder.sh" ''
          source "$stdenv/setup"
          mkdir -p $out

          export DEBDIR=$TMPDIR/${pkgName}_${version}-1

          # make debian control file structure
          mkdir -p $DEBDIR/DEBIAN
          cp ${control} $DEBDIR/DEBIAN/control
          cp ${deb-copyright} $DEBDIR/DEBIAN/copyright
          cp $src $DEBDIR/DEBIAN/changelog
          cp ${deb-pre-install}  $DEBDIR/DEBIAN/preinst
          cp ${deb-post-install}  $DEBDIR/DEBIAN/postinst
          cp ${deb-pre-rm}  $DEBDIR/DEBIAN/prerm
          cp ${deb-post-rm}  $DEBDIR/DEBIAN/postrm

          # make install file structure
          mkdir -p $DEBDIR/usr/bin
          mkdir -p $DEBDIR/etc/${pkgName}
          mkdir -p $DEBDIR/etc/sysctl.d
          mkdir -p $DEBDIR/etc/udev/rules.d
          mkdir -p $DEBDIR/lib/systemd/system/
          mkdir -p $DEBDIR/${root-dir}/{nix,dev,proc,sys,etc,run,usr,var,bin,lib,lib64,home,tmp}
          mkdir -p $DEBDIR/${exe-dir}

          ln -s ${obApp.exe}/* $DEBDIR/${exe-dir}/

          cp ${run-kiln-exe}/bin/* $DEBDIR/usr/bin/

          cp ${serviceFiles} $DEBDIR/lib/systemd/system/${pkgName}.service
          echo 'kernel.unprivileged_userns_clone=1' > $DEBDIR/etc/sysctl.d/10-kiln-userns.conf
          cp ${udevRules}  $DEBDIR/etc/udev/rules.d/20-kiln-ledger.rules

          # User can modify this to specify optional args like --network, --port
          echo "KILNARGS=" > $DEBDIR/etc/${pkgName}/args

          # copy nix closure
          storePaths=$(${pkgs.perl}/bin/perl ${pkgs.pathsFromGraph} closure)
          mkdir -p $DEBDIR/${nix-store-root}/nix/store
          cp -prd $storePaths $DEBDIR/${nix-store-root}/nix/store/

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
         adduser --quiet kiln plugdev
         chown -R kiln /var/lib/kiln
         service procps start
         udevadm trigger
         udevadm control --reload-rules
    esac
    if [ -d /run/systemd/system ]; then
        systemctl --system daemon-reload >/dev/null
        systemctl enable kiln.service >/dev/null
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

  udevRules = pkgs.writeTextFile { name = "20-kiln-ledger.rules"; text = ''
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1b7c", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="2b7c", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="3b7c", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="4b7c", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1807", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1808", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0000", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0004", MODE="0660", GROUP="plugdev"
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
      # The 'ln obApp.exe' is redundant, but added here to force obApp to be in closure of
      # run-kiln-exe. Without this we dont get obApp in deb
      # Not using writeScriptBin here, as we want to use /bin/bash
      run-kiln-backend-script = ''
        #!/usr/bin/env bash
        cd '${exe-dir}'
        ln -s ${obApp.exe}/* .
        ./backend --kiln-data-dir='${data-dir}' \$@
      '';

      kiln-shell-mainnet-rc = pkgs.writeText "bashrc" ''
        function tezos-client {
          unshare --mount --map-root-user kiln-do-mount-and-pivot ${nodeKit}/bin/mainnet-tezos-client $@
        }
        function tezos-admin-client {
          unshare --mount --map-root-user kiln-do-mount-and-pivot ${nodeKit}/bin/mainnet-tezos-admin-client $@
        }
      '';

      kiln-shell-alphanet-rc = pkgs.writeText "bashrc" ''
        function tezos-client {
          unshare --mount --map-root-user kiln-do-mount-and-pivot ${nodeKit}/bin/alphanet-tezos-client $@
        }
        function tezos-admin-client {
          unshare --mount --map-root-user kiln-do-mount-and-pivot ${nodeKit}/bin/alphanet-tezos-admin-client $@
        }
      '';
      kiln-shell-zeronet-rc = pkgs.writeText "bashrc" ''
        function tezos-client {
          unshare --mount --map-root-user kiln-do-mount-and-pivot ${nodeKit}/bin/zeronet-tezos-client $@
        }
        function tezos-admin-client {
          unshare --mount --map-root-user kiln-do-mount-and-pivot ${nodeKit}/bin/zeronet-tezos-admin-client $@
        }
      '';

      # Since gargoyle (or rather postgresql) can only work if invoked by a non-root user
      # We need to do a nested unshare (after doing mount) to change to a non-root shell
      # But the second 'unshare --user' (to go from root -> nobody) cannot happen after doing chroot
      # (see error EPERM, in man 2 unshare)
      # 
      # So in order to do a nested unshare we instead do 'pivot_root'
      kiln-do-mount-and-pivot = ''
        #!/usr/bin/env bash
        mount --bind '${root-dir}'  '${root-dir}'
        mount --rbind /proc  '${root-dir}/proc'
        mount --rbind --make-unbindable '${nix-store-root}/nix'   '${root-dir}/nix'
        mount --rbind --make-unbindable /dev   '${root-dir}/dev'
        mount --rbind --make-unbindable /sys   '${root-dir}/sys'
        mount --rbind --make-unbindable /etc   '${root-dir}/etc'
        mount --rbind --make-unbindable /run   '${root-dir}/run'
        mount --rbind --make-unbindable /usr   '${root-dir}/usr'
        mount --rbind --make-unbindable /var   '${root-dir}/var'
        mount --rbind --make-unbindable /bin   '${root-dir}/bin'
        mount --rbind --make-unbindable /lib   '${root-dir}/lib'
        mount --rbind --make-unbindable /lib64 '${root-dir}/lib64'
        mount --rbind --make-unbindable /home   '${root-dir}/home'
        mount --rbind --make-unbindable /tmp   '${root-dir}/tmp'
        mkdir -p '${root-dir}/oldroot'
        cd '${root-dir}'
        pivot_root . oldroot
        cd /
        umount -l oldroot
        exec unshare --user \$1 ${argStr}
      '';
      argStr = pkgs.lib.strings.escapeNixString "\${@:2}";

      run-kiln-backend = ''
        #!/usr/bin/env bash
        exec unshare --mount --map-root-user kiln-do-mount-and-pivot run-kiln-backend-script \$@
      '';

      kiln-shell = ''
        #!/usr/bin/env bash
        mkdir -p /tmp/kiln-shell-home
        if [[ \$# -eq 0 ]] ; then
        	echo \"Starting kiln-shell for mainnet.\"
        	echo \"To run kiln-shell for other network, please specify 'kiln-shell alphanet' or 'kiln-shell zeronet'.\"
          bash --rcfile ${nix-store-root}/${kiln-shell-mainnet-rc}
        else
        	case \$1 in
        		mainnet)
        			echo \"Starting kiln-shell for mainnet.\"
              bash --rcfile ${nix-store-root}/${kiln-shell-mainnet-rc}
        			;;
        		zeronet)
        			echo \"Starting kiln-shell for zeronet.\"
              bash --rcfile ${nix-store-root}/${kiln-shell-zeronet-rc}
        			;;
        		alphanet)
        			echo \"Starting kiln-shell for alphanet.\"
              bash --rcfile ${nix-store-root}/${kiln-shell-alphanet-rc}
        			;;
        		*)
        			echo \"Unknown argument, specify mainnet, zeronet or alphanet\"
        			exit 1
        			;;
        	esac
        fi
      '';

    in pkgs.runCommand "run-kiln-exe" {
        dontPatchShebangs = true;
      } ''
        mkdir -p $prefix/bin
        echo -n "${kiln-do-mount-and-pivot}" > $prefix/bin/kiln-do-mount-and-pivot
        echo -n "${run-kiln-backend}" > $prefix/bin/run-kiln-backend
        echo -n "${kiln-shell}" > $prefix/bin/kiln-shell
        echo -n "${run-kiln-backend-script}" > $prefix/bin/run-kiln-backend-script
        chmod +x $prefix/bin/*
      '';

  # Also see obelisk/default.nix systemd.services, ideally these two should be in sync somehow
  serviceFiles = pkgs.writeTextFile { name = "${pkgName}.service"; text = ''
    [Unit]
    Description=Kiln

    [Service]
    Type=simple
    EnvironmentFile=/etc/${pkgName}/args
    ExecStart=/usr/bin/run-kiln-backend $KILNARGS
    Restart=always
    RestartSec=5
    User=kiln

    [Install]
    WantedBy=multi-user.target
    After=network.target
  ''; };
in {
  inherit kiln-debian;
}
