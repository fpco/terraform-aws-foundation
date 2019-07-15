#!/usr/bin/env bash

set -o pipefail

# start CPU stress on two ASG nodes
ssh -f -F ssh_config asg-server-1-sysadmin 'nohup stress -c 2 -t 360s &'
ssh -f -F ssh_config asg-server-2-sysadmin 'nohup stress -c 2 -t 360s &'
