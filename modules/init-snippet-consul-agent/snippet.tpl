# start snippet - consul agent
${init_prefix}

cat <<EOT >> ${bootstrap_pillar_file}
# setup consul
consul:
  client_token: ${consul_client_token}
  datacenter: ${datacenter}
  disable_remote_exec: ${disable_remote_exec}
  secret_key: ${consul_secret_key}
  leaders:
    - ${leader_dns}
  webui: ${consul_webui}
  retry_interval: ${retry_interval}
EOT

echo "${log_prefix} restart dnsmasq to be sure it is online"
service dnsmasq restart
echo "${log_prefix} apply the consul.service salt formula to run a leader"
salt-call --local state.sls consul.service --log-level=${log_level}
${init_suffix}
