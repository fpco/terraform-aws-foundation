echo "setup openntpd in server mode"
salt-call --local state.sls openntpd $VERBOSE
