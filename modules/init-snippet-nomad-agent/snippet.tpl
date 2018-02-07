# start snippet - nomad agent
${init_prefix}

cat <<EOT >> ${bootstrap_pillar_file}
# pillar for nomad.service formula
${nomad_pillar}
EOT

echo "${log_prefix} setup and run the nomad agent"
salt-call --local state.sls nomad.service --log-level=${log_level}
${init_suffix}
