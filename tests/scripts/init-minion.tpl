#!/bin/sh
cat <<EOT > /srv/pillar/bootstrap.sls
salt:
  minion:
    # the service key is used by the formula
    service:
      status: dead
      enabled: False
    # these keys go into the minion's config
    #master: salt
    #id: basehost

consul:
  datacenter: ${region}
  secret_key: ${secret_key}
  leaders:
    - ${leader_dns}
  master_token: ${master_token}

EOT

ufw disable 
salt-call --local state.sls salt.file_roots.multiple >> /var/log/hello_world.log
salt-call --local state.sls salt.minion.base >> /var/log/hello_world.log
salt-call --local state.sls consul.agent >> /var/log/hello_world.log
dig ${leader_dns} >> /var/log/hello_world.log

