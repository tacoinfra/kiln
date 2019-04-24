To build Kiln distributions you must be running on Linux or have at least one Linux remote builder configured.

## Building the Docker image

```shell
nix-build -A dockerImage --no-out-link
```

The result of this command will be the path to a Docker image. You can load it with `docker load -i <path>`.

## Building the Debian Package


```shell
nix-build -A kiln-debian
```

The result of this command will be the path to the deb file.