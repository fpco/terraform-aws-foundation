export CLIENT_NAME="${client_name}"
export DEPOT_PATH="${depot_path}"
export CA_CONFIG="./ca_config.json"
export CA_COUNTRY="${ca_country}"
export CA_STATE="${ca_state}"
export CA_LOCALITY="${ca_locality}"
export CA_COMMON_NAME="${ca_common_name}"
export CA_PASSPHRASE="${ca_passphrase}"
export CA_DOMAIN_NAME="${domain_name}"
export SERVER_CONTEXT="${server_context}"
export CLIENT_CONTEXT="${client_context}"
export CREDSTASH_PUT_COMMAND="${credstash_put_cmd}"

./generate-ca-cert.sh
./generate-nomad-certs.sh
./generate-consul-certs.sh
