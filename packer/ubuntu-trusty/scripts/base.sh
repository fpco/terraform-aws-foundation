#!/bin/sh

# Update apt cache, avoids problems when we install packages
apt-get -y update
apt-get -y upgrade
