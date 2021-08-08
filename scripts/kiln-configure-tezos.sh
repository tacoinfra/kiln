#!/usr/bin/env bash
OS=$(uname)

write_paths_config() {
    mkdir -p $CONFIGDIR
    tee $CONFIGDIR/binary-paths > /dev/null << EOF
{
    "node-path" : "$TEZDIR/tezos-node"
    , "client-path" : "$TEZDIR/tezos-client"
    , "baker-endorser-paths" :
        [
    ["PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i","$TEZDIR/tezos-baker-009-PsFLoren","$TEZDIR/tezos-endorser-009-PsFLoren"],
    ["PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV","$TEZDIR/tezos-baker-010-PtGRANAD","$TEZDIR/tezos-endorser-010-PtGRANAD"]
        ]
}
EOF
}

TEZVER=${1-"9.7"}

case $OS in

    Darwin)
        KILNDIR=$HOME/Library/Kiln
        CONFIGDIR=$KILNDIR/config
        TEZDIR=$KILNDIR/"tezos-$TEZVER"

        download() {
	    ver=v${TEZVER}-1
            binary=$1
            archive=$binary-$ver.mojave.bottle.tar.gz
            url=https://github.com/serokell/tezos-packaging/releases/download/$ver/$archive
            echo "Downloading $binary from $url"
            curl -L $url -o $archive
            tar --strip-components 3 -xvf $archive $binary/$ver/bin/$binary
            rm $archive
        }

        restart() {
            launchctl stop tezos.kiln
            sleep 5
            launchctl start tezos.kiln
        }

        ;;

    Linux)
        KILNDIR=/var/lib/kiln/
        CONFIGDIR=$KILNDIR/exe-dir/config
        TEZDIR=$KILNDIR/"tezos-$TEZVER"
        apt install curl

        download() {
            binary=$1
            url=https://gitlab.com/api/v4/projects/3836952/packages/generic/tezos/${TEZVER}.0/x86_64-$binary
            echo "Downloading $binary from $url"
            curl -L $url -o $binary
        }

        restart() {
            systemctl restart kiln
        }

        ;;

    *)
        echo "Unsupported operating system $OS"
        exit 1
        ;;
esac

#download tezos binaries
mkdir -p $TEZDIR
(cd $TEZDIR &&
     for binary in "tezos-client" \
		       "tezos-node" \
		       "tezos-endorser-010-PtGRANAD" \
		       "tezos-baker-010-PtGRANAD" \
		       "tezos-endorser-009-PsFLoren" \
		       "tezos-baker-009-PsFLoren"
     do
         if [ ! -f ./$binary ];
         then
             download $binary
         else
             echo "$binary is already downloaded"
	 fi
     done
)

#make all tezos binaries executable
chmod +x $TEZDIR/*

#tell kiln about new binaries
write_paths_config

#restart kiln for config to take effect
restart
