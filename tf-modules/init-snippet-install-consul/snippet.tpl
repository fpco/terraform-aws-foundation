# start snippet - install consul
${init_prefix}
wget ${base_url}/${version}/consul_${version}_linux_amd64.zip
wget ${base_url}/${version}/consul_${version}_web_ui.zip
unzip consul_${version}_linux_amd64.zip -d /usr/local/bin/
consul version
useradd --user-group           \
        --system               \
        --shell /bin/bash      \
        --create-home          \
        --home-dir ${data_dir} \
        consul 
mkdir ${data_dir}/ui
unzip consul_${version}_web_ui.zip -d ${data_dir}/ui
${init_suffix}
