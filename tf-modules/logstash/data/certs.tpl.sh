#!/bin/bash

if [ -z "${depot_path}" ]; then
  DEPOT_PATH=$$(mktemp -d -t "logstash-XXXXXX")
else
  DEPOT_PATH="${depot_path}"
fi

certstrap --depot-path "$${DEPOT_PATH}" init --common-name "${ca_common_name}" --passphrase "${ca_passphrase}"
certstrap --depot-path "$${DEPOT_PATH}" request-cert --domain "${domain_name}" --passphrase ""
certstrap --depot-path "$${DEPOT_PATH}" sign "${domain_name}" --CA "${ca_common_name}" --passphrase "${ca_passphrase}"

# Convert into PKCS8 format.

openssl pkcs8 -topk8 -inform PEM -outform PEM -nocrypt -in "$${DEPOT_PATH}/${domain_name}.key" -out "$${DEPOT_PATH}/${domain_name}.pkcs8.key"

${credstash_put_cmd} -a "${ca_cert_name}" "@$${DEPOT_PATH}/${ca_common_name}.crt"
${credstash_put_cmd} -a "${ca_key_name}" "@$${DEPOT_PATH}/${ca_common_name}.key"
${credstash_put_cmd} -a "${server_cert_name}" "@$${DEPOT_PATH}/${domain_name}.crt"
${credstash_put_cmd} -a "${server_key_name}" "@$${DEPOT_PATH}/${domain_name}.pkcs8.key"

# Remove temporary folder with generated certificates
if [ -z "${depot_path}" ]; then
  chmod -R u+Xrw "$${DEPOT_PATH}"
  rm -R "$${DEPOT_PATH}"
fi
