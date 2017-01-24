#!/bin/bash

if [ -f /.dockerenv ]; then
  VPN_TUNNEL=/etc/init.d/vpn-tunnel
else
  VPN_TUNNEL=./vpn-tunnel
fi
test -z "${VPN_TUNN}" && VPN_TUNN="tun0"
INFO="
Usage: $(basename "$0") [INTERVAL]

This script will start VPN tunnel using $VPN_TUNNEL and will check
it's health every INTERVAL seconds (default 1 sec.) and restart it as necessary.
"
if [ "$1" = "--help" ]; then
  echo "$INFO"
  $VPN_TUNNEL | tail -n +4
  exit 0
fi
# Global variables
TEST_HOST="docker.e1c.net"
STATUS_IF=""
COUNT=3
INTERVAL=1
if [ -n "$1" ]; then
  INTERVAL=$1
fi
VPN_TUNNEL=/etc/init.d/vpn-tunnel

# Check if VPN's interface exist in the current environment, otherwise start VPN
function check_interface(){
  STATUS_IF=$(ip addr show | grep "${VPN_TUNN}" | awk -F: '{print $2}')
  test -z "$STATUS_IF" && $VPN_TUNNEL restart ; sleep 2    # give it some time to restart
}

# Check if interface is in place, ping host defined in global variables and check for echo-reply
# Restart VPN's in case there is no response from ICMP packets
function check_vpn(){

  if [ "$STATUS_IF" = "${VPN_TUNN}" ]; then
    count=$(ping -c ${COUNT} ${TEST_HOST} | grep 'received' \
               | awk -F',' '{ print $2 }' | awk '{ print $1 }')
    test "$count" = "0" && $VPN_TUNNEL restart
    # give it some time to restart
    sleep 2
  fi

}

trap "$VPN_TUNNEL stop; exit 0" SIGTERM
trap "$VPN_TUNNEL kill; exit 1" SIGKILL

$VPN_TUNNEL start

if [ "$?" = "2" ]; then
  echo "Missing required configuration. Terminating." >&2
  exit 1
fi

while true; do
  sleep $INTERVAL
  check_interface
  check_vpn
done
