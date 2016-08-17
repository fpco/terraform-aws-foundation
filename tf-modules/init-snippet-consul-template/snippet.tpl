# start snippet - consul template
${init_prefix}

cat <<EOT >> ${bootstrap_pillar_file}
# pillar for consul.template-tool.service formula
consul_template:
  consul_addr: ${consul_addr}
  client_token: ${consul_client_token}
  log_level: ${log_level}
EOT

echo "${log_prefix} configure/restart consul-template service"
salt-call --local state.sls consul.template-tool.service --log-level=${log_level}

${init_suffix}
