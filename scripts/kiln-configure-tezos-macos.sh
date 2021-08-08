#!/usr/bin/env bash
TEZVER=${1-"9.7-1"}

#go to kiln directory
cd ~/Library/Kiln/

#download tezos binaries
TEZDIR="tezos-$TEZVER"
mkdir -p $TEZDIR
cd $TEZDIR
for binary in "tezos-client" \
		  "tezos-node" \
		  "tezos-endorser-010-PtGRANAD" \
		  "tezos-baker-010-PtGRANAD" \
		  "tezos-endorser-009-PsFLoren" \
		  "tezos-baker-009-PsFLoren"
do
    if [ ! -f ./$binary ]; then
        archive=$binary-v$TEZVER.mojave.bottle.tar.gz
        url=https://github.com/serokell/tezos-packaging/releases/download/v$TEZVER/$archive
	echo "Downloading $binary from $url"
	curl -L $url --output $archive
        tar --strip-components 3 -xvf $archive $binary/v$TEZVER/bin/$binary
        rm $archive
    fi
done

#make all tezos binaries executable
chmod +x ./*

#tell kiln about new binaries
cd ..
mkdir -p config

TEZPATH=$HOME/Library/Kiln/$TEZDIR
#this creates file named "binary-paths" in config directory with JSON config that describes tezos binaries
tee config/binary-paths > /dev/null << EOF
{
    "node-path" : "$TEZPATH/tezos-node"
    , "client-path" : "$TEZPATH/tezos-client"
    , "baker-endorser-paths" :
        [
    ["PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i","$TEZPATH/tezos-baker-009-PsFLoren","$TEZPATH/tezos-endorser-009-PsFLoren"],
    ["PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV","$TEZPATH/tezos-baker-010-PtGRANAD","$TEZPATH/tezos-endorser-010-PtGRANAD"]
  ]
}
EOF

#restart kiln for config to take effect
launchctl stop tezos.kiln
sleep 5
launchctl start tezos.kiln
