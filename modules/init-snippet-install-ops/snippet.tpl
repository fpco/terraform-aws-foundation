# start snippet - install ops
${init_prefix}
wget -O /usr/local/bin/ops ${base_url}/ops-v${version}-linux-amd64
chmod +x /usr/local/bin/ops
ops --help
${init_suffix}
