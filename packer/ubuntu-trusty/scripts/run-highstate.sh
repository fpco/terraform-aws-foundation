#!/bin/sh

# for logging when running salt-call
VERBOSE="--log-level=debug"
# install pip before running highstate, so it is available from the get-go
salt-call --local state.sls python.pip $VERBOSE
# apply all states in top.sls from salt formula repos
salt-call --local state.highstate $VERBOSE
service salt-minion stop
# apply fail2ban.config from fail2ban formula repo
salt-call --local state.sls fail2ban.config $VERBOSE
