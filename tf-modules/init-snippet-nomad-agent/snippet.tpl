# start snippet - nomad agent
${init_prefix}

cat <<EOT >> ${bootstrap_pillar_file}
# pillar for nomad.service formula
nomad:
  server: ${server}
  datacenter: ${name}.${region}
  region: ${region}
  secret: ${nomad_secret}
  consul:
    token: ${consul_client_token}
  options:
    driver.raw_exec.enable: ${enable_raw_exec}

EOT

echo "${log_prefix} setup and run the nomad agent"
$APPLY_FORMULA nomad.service,nomad.nomad_check_agent --log-level=${log_level}
${init_suffix}
