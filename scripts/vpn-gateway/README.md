# Contents:

* `vpn-tunnel` - script that creates and tears down the connection (meant to be run inside docker)
* `vpn-runner.sh` - script that initializes the gateway and continuosly checks
  it's health, restarts as necessary. (meant to be run inside docker)
* `Dockerfile` - used to build the docker container
* `vpn-gateway` - script that daemonizes the vpn-tunnel running inside the docker container.

# Building

Building a docker container:

```bash
$ docker build . --tag=vpn-gateway # <- default tag name
```

# Running under docker.


```bash
$ docker run --net=host --privileged --rm -v $(dirname $(readlink -f /etc/resolv.conf)):/etc-host \
    -e VPN_USER="username" -e VPN_PASS="password" \
    -e VPN_HOST="connect.example.com" -e VPC_CIDR="172.16.0.0/16" \
    vpn-gateway vpn-runner.sh
```

`$(dirname $(readlink -f /etc/resolv.conf))` will resolve the directory were the
actual `resolv.conf` is located (docker cannot follow links to paths that aren't
mounted).

# Running as a service (Ubuntu)

## Credentials

Place configuration into `/etc/vpn-gateway.conf` in format:

```
docker-image repo/vpn-gateway:latest
vpc-cidr 172.16.0.0/16
hostname connect.example.com
username actual-vpn-username
password actual-vpn-password
```

Make sure the file is owned by `root` and no-one can access it:

```bash
$ sudo chown root:root /etc/vpn-gateway.conf 
$ sudo chmod 640 /etc/vpn-gateway.conf 
```

## Install service

Place the script into `/etc/init.d/`:
```bash
$ sudo cp vpn-gateway /etc/init.d/
$ sudo update-rc.d vpn-gateway defaults
$ sudo service vpn-gateway start
```

## Usage

Now it is possible to use VPN Gateway as a service:

```bash
$ sudo service vpn-gateway start # to start the gateway
$ sudo service vpn-gateway state # check docker status
$ sudo service vpn-gateway status # status of the service
$ sudo service vpn-gateway stop # gracefully stop the service
$ sudo service vpn-gateway restart # gracefully restart the service
```

## Uninstall service

```bash
$ sudo service vpn-gateway stop
$ sudo update-rc.d vpn-gateway remove
$ sudo rm /etc/init.d/vpn-gateway
```

