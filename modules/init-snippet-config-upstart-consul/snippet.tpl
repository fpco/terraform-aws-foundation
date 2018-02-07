# start snippet - configure upstart to run consul
${init_prefix}
cat <<EOT >> ${upstart_config}
description "Consul agent"

start on started networking
stop on runlevel [!2345]

respawn
# This is to avoid Upstart re-spawning the process upon `consul leave`
normal exit 0 INT
setuid ${consul_user}
script
  exec ${consul_bin} agent -config-file=${config_file} -config-dir=${config_dir}
end script
EOT
${init_suffix}
