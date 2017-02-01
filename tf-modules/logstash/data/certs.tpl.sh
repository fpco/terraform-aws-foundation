#!/bin/bash
certstrap --depot-path "${depot_path}" init --common-name "${ca_common_name}" --passphrase "${ca_passphrase}"
certstrap --depot-path "${depot_path}" request-cert --domain "${domain_name}" --passphrase ""
certstrap --depot-path "${depot_path}" sign "${domain_name}" --CA "${ca_common_name}" --passphrase "${ca_passphrase}"

# Convert into PKCS8 format.

openssl pkcs8 -topk8 -inform PEM -outform PEM -nocrypt -in "${depot_path}/${domain_name}.key" -out "${depot_path}/${domain_name}.pkcs8.key"

${credstash_put_cmd} -a "${ca_cert_name}" "@${depot_path}/${ca_common_name}.crt"
${credstash_put_cmd} -a "${ca_key_name}" "@${depot_path}/${ca_common_name}.key"
${credstash_put_cmd} -a "${server_cert_name}" "@${depot_path}/${domain_name}.crt"
${credstash_put_cmd} -a "${server_key_name}" "@${depot_path}/${domain_name}.pkcs8.key"
