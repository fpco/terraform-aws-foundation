# start snippet - consul leader
${init_prefix}
cat <<EOT >> ${bootstrap_pillar_file}
# setup consul
consul:
  datacenter: ${datacenter}
  disable_remote_exec: ${disable_remote_exec}
  secret_key: '${secret_key}'
  master_token: '${master_token}'
  client_token: '${client_token}'
  leader_count: '${leader_count}'
  retry_interval: '3s'
  leaders:
    - ${cidr_prefix_a}.4
    - ${cidr_prefix_c}.4
    - ${cidr_prefix_a}.5
    - ${cidr_prefix_c}.5
    - ${cidr_prefix_a}.6
    - ${cidr_prefix_c}.6
    - ${cidr_prefix_a}.7
    - ${cidr_prefix_c}.7
    - ${cidr_prefix_a}.8
    - ${cidr_prefix_c}.8
    - ${cidr_prefix_a}.9
    - ${cidr_prefix_c}.9
    - ${cidr_prefix_a}.10
    - ${cidr_prefix_c}.10
    - ${cidr_prefix_a}.11
    - ${cidr_prefix_c}.11
    - ${cidr_prefix_a}.12
    - ${cidr_prefix_c}.12
    - ${cidr_prefix_a}.13
    - ${cidr_prefix_c}.13
    - ${cidr_prefix_a}.14
    - ${cidr_prefix_c}.14
  webui: ${consul_webui}
EOT

echo "${log_prefix} restart dnsmasq to be sure it is online"
service dnsmasq restart
echo "${log_prefix} apply the consul.service salt formula to run a leader"
salt-call --local state.sls consul.service --log-level=${log_level}
${init_suffix}
