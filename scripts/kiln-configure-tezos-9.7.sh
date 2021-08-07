#!/usr/bin/env bash

#go to kiln directory
cd /var/lib/kiln/

#download tezos binaries
mkdir -p tezos-9.7
cd tezos-9.7/
for binary in "tezos-client" \
		  "tezos-node" \
		  "tezos-endorser-010-PtGRANAD" \
		  "tezos-baker-010-PtGRANAD" \
		  "tezos-endorser-009-PsFLoren" \
		  "tezos-baker-009-PsFLoren"
do
    if [ ! -f ./$binary ]; then
	url=https://gitlab.com/api/v4/projects/3836952/packages/generic/tezos/9.7.0/x86_64-$binary
	echo "Downloading $binary from $url"
	wget $url  -O $binary
    fi
done

#make all tezos binaries executable
chmod +x ./*

#tell kiln about new binaries
cd ../exe-dir
mkdir -p config

#this creates file named "binary-paths" in config directory with JSON config that describes tezos binaries
tee config/binary-paths > /dev/null << EOF
{
    "node-path" : "/var/lib/kiln/tezos-9.7/tezos-node"
    , "client-path" : "/var/lib/kiln/tezos-9.7/tezos-client"
    , "baker-endorser-paths" :
        [
    ["PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i","/var/lib/kiln/tezos-9.7/tezos-baker-009-PsFLoren","/var/lib/kiln/tezos-9.7/tezos-endorser-009-PsFLoren"],
    ["PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV","/var/lib/kiln/tezos-9.7/tezos-baker-010-PtGRANAD","/var/lib/kiln/tezos-9.7/tezos-endorser-010-PtGRANAD"]
  ]
}
EOF

#restart kiln for config to take effect
systemctl restart kiln
