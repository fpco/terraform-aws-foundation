FROM fpco/pid1
ENTRYPOINT ["/sbin/pid1"]

COPY vpn-tunnel /etc/init.d/vpn-tunnel
COPY vpn-runner.sh /usr/local/bin/vpn-runner.sh

RUN chmod a+x /etc/init.d/vpn-tunnel && chmod a+x /usr/local/bin/vpn-runner.sh
RUN apt-get update && DEBIAN_FRONTEND=noninteractive \
    apt-get install -y --no-install-recommends apt-utils \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get -y update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    openconnect iptables iproute2 net-tools vpnc-scripts iputils-ping 
