# start snippet - write bootstrap pillar
${init_prefix}
cat <<EOT >> ${bootstrap_pillar_file}
${pillar}
EOT
${init_suffix}
