# start snippet - install consul
${init_prefix}
wget "${base_url}/${consul_version}/consul_${consul_version}_linux_amd64.zip"
unzip "consul_${consul_version}_linux_amd64.zip" -d /usr/local/bin/
consul version
useradd --user-group           \
        --system               \
        --shell /bin/bash      \
        --create-home          \
        --home-dir "${data_dir}" \
        consul
${init_suffix}
