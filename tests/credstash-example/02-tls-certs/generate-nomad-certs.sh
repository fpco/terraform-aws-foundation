#!/bin/bash
set -x

CLIENT_NAME="${client_name}"
DEPOT_PATH="${depot_path}"
CA_CONFIG="./ca_config.json"
CA_COUNTRY="${ca_country}"
CA_STATE="${ca_state}"
CA_LOCALITY="${ca_locality}"
CA_COMMON_NAME="${ca_common_name}"
CA_PASSPHRASE="${ca_passphrase}"
CA_DOMAIN_NAME="${domain_name}"
SERVER_CONTEXT="${server_context}"
CLIENT_CONTEXT="${client_context}"
CREDSTASH_PUT_COMMAND="${credstash_put_cmd}"

# Remove any old certificate
rm -f "$DEPOT_PATH"

## Create Server SSL Key and Certificate
#certstrap --depot-path "$DEPOT_PATH" request-cert --domain "$DOMAIN_NAME" --passphrase "$CA_PASSPHRASE"
#certstrap --depot-path "$DEPOT_PATH" sign "$DOMAIN_NAME" --CA "$CA_COMMON_NAME" --passphrase "$CA_PASSPHRASE"
echo '{}' | cfssl gencert -ca=$CA_COMMON_NAME-ca.pem -ca-key=$CA_COMMON_NAME-ca-key.pem -config=cfssl.json -hostname="server.${REGION}.nomad,localhost,127.0.0.1" - | cfssljson -bare server

## Convert Server Key into PKCS8 format.
#openssl pkcs8 -topk8 -inform PEM -outform PEM -nocrypt -in "$DEPOT_PATH/$DOMAIN_NAME.key" -out "$DEPOT_PATH/$DOMAIN_NAME.pkcs8.key"

## Create Client SSL Key and Certificate
#certstrap --depot-path "$DEPOT_PATH" request-cert --domain "$CLIENT_NAME" --passphrase ""
#certstrap --depot-path "$DEPOT_PATH" sign "$CLIENT_NAME" --CA "$CA_COMMON_NAME" --passphrase "$CA_PASSPHRASE"
echo '{}' | cfssl gencert -ca=$CA_COMMON_NAME-ca.pem -ca-key=$CA_COMMON_NAME-ca-key.pem -config=cfssl.json -hostname="client.${REGION}.nomad,localhost,127.0.0.1" - | cfssljson -bare client

## Convert Client Key into PKCS8 format.
#openssl pkcs8 -topk8 -inform PEM -outform PEM -nocrypt -in "$DEPOT_PATH/$CLIENT_NAME.key" -out "$DEPOT_PATH/$CLIENT_NAME.pkcs8.key"

## Create TLS key for CLI use
echo '{}' | cfssl gencert -ca=$CA_COMMON_NAME-ca.pem -ca-key=$CA_COMMON_NAME-ca-key.pem -profile=client  - | cfssljson -bare cli

## Store all certificates an keys with credstash
#$CREDSTASH_PUT_CMD -a ca_key "@$DEPOT_PATH/$CA_COMMON_NAME.key"
#$CREDSTASH_PUT_CMD -a server_ca_cert "@$DEPOT_PATH/$CA_COMMON_NAME.crt" $SERVER_CONTEXT
#$CREDSTASH_PUT_CMD -a server_cert "@$DEPOT_PATH/$DOMAIN_NAME.crt" $SERVER_CONTEXT
#$CREDSTASH_PUT_CMD -a server_key "@$DEPOT_PATH/$DOMAIN_NAME.pkcs8.key" $SERVER_CONTEXT

#$CREDSTASH_PUT_CMD -a client_ca_cert "@$DEPOT_PATH/$CA_COMMON_NAME.crt" $CLIENT_CONTEXT
#$CREDSTASH_PUT_CMD -a client_cert "@$DEPOT_PATH/$CLIENT_NAME.crt" $CLIENT_CONTEXT
#$CREDSTASH_PUT_CMD -a client_key "@$DEPOT_PATH/$CLIENT_NAME.pkcs8.key" $CLIENT_CONTEXT
