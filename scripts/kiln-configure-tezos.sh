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
MAJORVER=$(echo $TEZVER | cut -d "." -f 1)
MAINNET_CHAIN_ID="NetXdQprcVkpaWU"

case $OS in

    Darwin)
        KILNDIR=$HOME/Library/Kiln
        CONFIGDIR=$KILNDIR/config
        TEZDIR=$KILNDIR/"tezos-$TEZVER"
        DATA_DIR=$KILNDIR/.kiln/tezos-node/$MAINNET_CHAIN_ID
	export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/kiln-nix/lib/

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

        upgrade_storage() {
            $TEZDIR/tezos-node upgrade --data-dir $DATA_DIR storage
        }

        stop() {
            launchctl stop tezos.kiln
        }

        start() {
            launchctl start tezos.kiln
        }

        ;;

    Linux)
        KILNDIR=/var/lib/kiln
        CONFIGDIR=$KILNDIR/exe-dir/config
        TEZDIR=$KILNDIR/"tezos-$TEZVER"
        DATA_DIR=$KILNDIR/data-dir/tezos-node/$MAINNET_CHAIN_ID
        apt install curl

        download() {
            binary=$1
            url=https://gitlab.com/api/v4/projects/3836952/packages/generic/tezos/${TEZVER}.0/x86_64-$binary
            echo "Downloading $binary from $url"
            curl -L $url -o $binary
        }

        upgrade_storage() {
            sudo -u kiln $TEZDIR/tezos-node upgrade --data-dir $DATA_DIR storage
        }

        stop() {
            systemctl stop kiln
        }

        start() {
            systemctl start kiln
        }

        ;;

    *)
        echo "Unsupported operating system $OS"
        exit 1
        ;;
esac

FREE_SPACE=$(df -h $DATA_DIR/ | awk '$3 ~ /[0-9]+/ { print $4 }')

if [ $MAJORVER == "10" ]
then
    cat <<EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Tezos 10.x node requires storage migration.
Kiln node will be stopped for the duration of storage migration,
which may take anywhere between several minutes and several hours
depending on node's history mode (rolling or full) and your hardware.

Note that Tezos 10.x storage format cannot be converted back to 9.x,
so the only way to downgrade is to re-create Kiln node from a snapshot.

Tezos documentation recommends at least 10G of free space.
You have ${FREE_SPACE}.

See https://tezos.gitlab.io/releases/version-10.html#storage-upgrade
for more details.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
    read -p "Type Y to continue, anything else to exit: " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo "Updating Kiln to $TEZVER..."
    else
        echo "Update cancelled, exiting..."
        exit 1
    fi
fi


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

echo "Stopping Kiln..."
stop
sleep 5

if [ $MAJORVER == "10" ]
then
    echo "Running storage upgrade..."
    upgrade_storage
fi

echo "Starting Kiln..."
start

echo "Done."
