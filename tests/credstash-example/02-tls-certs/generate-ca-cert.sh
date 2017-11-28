#!/bin/bash
set -x

# Remove any old certificate
rm -f "$DEPOT_PATH"

## Write out config for CA cert generation
cat <<EOT > $CA_CONFIG
{
    "CN": "$CA_COMMON_NAME",
    "hosts": [
        "$CA_DOMAIN_NAME"
    ],
    "key": {
        "algo": "ecdsa",
        "size": 256
    },
    "names": [
        {
            "C": "$CA_COUNTRY",
            "ST": "$CA_STATE",
            "L": "$CA_LOCALITY"
        }
    ]
}
EOT

## Create CA SSL Key and Certificate
#certstrap --depot-path "$DEPOT_PATH" init --common-name "$CA_COMMON_NAME" --passphrase "$CA_PASSPHRASE"
cfssl gencert -initca $CA_CONFIG | cfssljson -bare $CA_COMMON_NAME-ca

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
