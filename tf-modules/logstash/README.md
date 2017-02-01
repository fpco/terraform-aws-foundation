## Dependencies

In order for Logstash Beat protocol to function there should be an SSL style
authentication setup in place. There are two tools utilized in order to make
this process semi-automatic, but those tools need to be installed.

### Credstash

This tool uses Amazon's KMS and DynamoDB to store and retrieve secret values, in
case of Logstash it will be server private SSL key.

* Github repo with usage instructions: [credstash](https://github.com/fugue/credstash)

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
deployment process. Unfortunately it also requires a cleanup action
