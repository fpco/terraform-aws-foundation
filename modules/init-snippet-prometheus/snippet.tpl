# start snippet - prometheus
${init_prefix}
cat <<EOT >> ${bootstrap_pillar_file}
${prometheus_pillar}
EOT

echo "${log_prefix} configure/start prometheus as a service"
salt-call --local state.sls prometheus.server --log-level=${log_level}
${init_suffix}
