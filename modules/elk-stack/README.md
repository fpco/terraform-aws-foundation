# Deployment of ELK stack

This module will deploy Elasticsearch cluster in a private subnet which must
consist of at least 1 master node and 1 data node, specified by
`elasticsearch_master_node_count`, `elasticsearch_data_node_count`. Logstash and
Kibana will be deployed on all other nodes in public subnet, which are controlled by
`logstash_kibana_min_server_count` and `logstash_kibana_max_server_count`.

While most of the deployment is automated, there are a few manual steps required.

## Local dependnecies



## Kibana DNS and SSL

Kibana DNS, which is set by `kibana_dns_name`, is assigned to an ELB. SSL
certificate matching the domain name is required for this to work properly thus
it must be created in ACM beforehand.

For development and testing the easiest way to do it is to upload a certificate
to [ACM](https://console.aws.amazon.com/acm/home) signed by a custom CA:

```shell
$ certstrap --depot-path=depot init --common-name=elk-dev --passphrase=""
Created depot/elk-dev.key
Created depot/elk-dev.crt
Created depot/elk-dev.crl
$ certstrap --depot-path=depot request-cert --domain=kibana-dev.full.name.net --passphrase=""
Created depot/kibana-dev.full.name.net.key
Created depot/kibana-dev.full.name.net.csr
$ certstrap --depot-path=depot sign kibana-dev.full.name.net" --CA=elk-dev
Created depot/kibana-dev.full.name.net.crt from depot/kibana-dev.full.name.net.csr signed by depot/elk-dev.key
```

In ACM you can just upload generated SSL:
* Certificate body - `depot/kibana-dev.full.name.net.crt`
* Certificate private key - `depot/kibana-dev.full.name.net.key`
* Certificate chain - `depot/elk-dev.crt`


## Credstash

Logstash will use certstrap to generate certificates in a similar fasion it is
done in the previous section. In order to use those keys automatically on the
AWS side, credstash will be used to securely store and retrieve private key
encrypted using KMS inside DynamoDB. Credstash requires manual KMS key creation,
which can be done manually an AWS Console or way simpler just by using
`tf-modules/credstash-setup` module. That module will create database table and
KMS key, `kms_key_arn` of which will be produced by terrraform as output and
needs to be supplied here as `credstash_kms_key_arn`:

```shell
$ cd fpco-terraform-aws/tf-modules/credstash-setup
$ terraform apply
...
$ terraform show | grep "arn =" | grep ":key/" | awk '{print $3}'
arn:aws:kms:us-east-1:123456789:key/b467efac-3c9f-11e7-82ae-078fb13d1789
```
