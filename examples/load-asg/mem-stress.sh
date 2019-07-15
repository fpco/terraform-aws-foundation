#!/usr/bin/env bash

set -o pipefail

# start Memory stress on two ASG nodes
ssh -f -F ssh_config asg-server-1-sysadmin 'nohup stress --vm 2 --vm-bytes 512M -t 360s &'
ssh -f -F ssh_config asg-server-2-sysadmin 'nohup stress --vm 2 --vm-bytes 512M -t 360s &'
