#!/usr/bin/env bash

# This script will need to be run as root

sudo yum -y update
sudo yum -y install python3-pip wget
pip3 install awscli --upgrade --user
sudo yum -y install sssd realmd krb5-workstation samba-common-tools

