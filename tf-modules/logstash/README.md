## Dependencies

In order for Logstash Beat protocol to function there should be an SSL style
authentication setup in place. There are two tools utilized in order to make
this process semi-automatic, but those tools need to be installed.

### Credstash

This tool uses Amazon's KMS and DynamoDB to store and retrieve secret values, in
case of Logstash it will be server private SSL key.

* Github repo with usage instructions: [credstash](https://github.com/fugue/credstash)

Once credstash is installed it also requires an initial setup (DynamoDB table
and KMS Master key created). For this purpose `credstash-setup` module can be
used, or it can be done manually by running `$ credstash setup`, which will
create DynamoDB table, and manually creating KMS key with alias 'credstash`
using AWS UI.

### Certstrap

This tool is used to create Certificate Authority (CA), which will be used for
server/client authentication. Logstash will have an SSL Server Certificate,
while any Beat app
([Filebeat](https://www.elastic.co/products/beats/filebeat),
[Metricbeat](https://www.elastic.co/products/beats/metricbeat),
[Packetbeat](https://www.elastic.co/products/beats/packetbeat)) will have a
client SSL key, both will be signed by the same CA. Therefore, with this setup,
client and server can authenticate each other when receiving/pushing logs.

* Github repo with usage instructions: [certstrap](https://github.com/square/certstrap)

## Cleanup

In order for credstash to operate on the EC2 instance, the role that it assumes
should be grated acces to the KMS, which is completely automated by the
deployment process. Unfortunately it also requires a cleanup action, which has
to be done manually until trerraform 9 is released. Cleanup shell command is one
of the outputs of this module and needs to be called before the
destruction. However, this cleanup consists of revoking grants created for roles
using credstash and is not critical, since roles are deleted during destruction,
and can be done manually at any time. Revoke all grants that don't have a valid
ARN as it's grantee.

```bash
# KMS_KEY_ARN on GovCloud has 'arn:aws-us-gov:…' instead of 'arn:aws:…'
$ KMS_KEY_ARN="arn:aws:kms:us-east-1:1234567890:key/b2fcd07b-..."
$ GRANT_IDS=$(aws kms list-grants --key-id $KMS_KEY_ARN | \
    jq -r '.Grants[]|select(.GranteePrincipal|startswith("arn:")|not).GrantId')
$ for GrantId in $GRANT_IDS; do \
      aws kms revoke-grant --key-id $KMS_KEY_ARN --grant-id $GrantId; \
    done
```

