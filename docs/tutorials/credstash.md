# Credstash

[credstash](https://github.com/fugue/credstash) is a command line tool that
leverages KMS to encrypt/decrypt secret information and DynamoDB to store the
data in encrypted form.

## Setup

In order to start using `credstash`, KMS Master Key and DynamoDB table need to
be created. Also, to get access to secrets in the store with credstash, IAM
roles and policies ought to be used.

[credstash-setup](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/credstash-setup) is
the module that can take care of the whole setup and need to be applied only
once. This module creates resources in the current region:

* DynamoDB table with default value: `credstash-store`
* KMS Master Key and an alias for it with default value: `alias/credstash`
* Reader and/or Writer IAM Policies that give access to created DynamoDB table.
  Those policies can later be assigned to a role assumed by an EC2 instance that
  will be reading/writing secrets with `credstash`. More on that in the next
  section.

Resources created during this setup can be used anywhere throughout your AWS
account as many times as you’d like, for this reason it should be setup in a
separate environment preferably with its terraform state stored in a remote
location like an s3 bucket. On the other hand if multiple tables and KMS keys
are used by credstash within the same AWS account, variables `kms_key_name` and
`db_table_name` must be customized. Here is an example of `credstash-setup`
module usage:

```hcl
terraform {
  backend "s3" {
    encrypt = "true"
    region  = "us-west-1"
    bucket  = "remote-tfstate"
    key     = "credstash/terraform.tfstate"
  }
}
module "credstash" {
  source               = "fpco/foundation/aws//modules/credstash-setup"
  create_reader_policy = true # can be ommitted if secrets are writeonly from within EC2
  create_writer_policy = true # can be ommitted if secrets are readonly from within EC2
}
# Below outputs will be used for automatic credstash usage by EC2 instances
output "kms_key_arn" {
  value = "${module.credstash.kms_key_arn}"
}
output "install_snippet" {
  value = "${module.credstash.install_snippet}"
}
output "get_cmd" {
  value = "${module.credstash.get_cmd}"
}
output "put_cmd" {
  value = "${module.credstash.put_cmd}"
}
output "reader_policy_arn" {
  value = "${module.credstash.reader_policy_arn}"
}
output "writer_policy_arn" {
  value = "${module.credstash.writer_policy_arn}"
}
```

## Manual usage

After all resources have been deployed with `terraform apply` we can start using `credstash`.

Here is a simple example on how to use it in the terminal:

```bash
$ credstash put my-secret poor-entropy-password
my-secret has been stored
$ credstash get my-secret
poor-entropy-password
```

Add a new version for existing key (`-a` for auto-increment) and list keys with
value version numbers:

```
$ credstash put -a my-secret high-entropy-password
my-secret has been stored
$ credstash get my-secret
high-entropy-password
$ credstash list
my-secret      -- version 0000000000000000000
my-secret      -- version 0000000000000000001
```

## Automated usage

Considering that roles and users mentioned in above section have administrative
level of access to KMS Key it would be really bad to use them in EC2 bootstrap
scripts. In order to narrow down permissions to `credstash-store` DynamoDB table
and restrict usage patterns to KMS Master
Key
[credstash-grant](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/credstash-grant) terrform
module should be used.

`credstash-grant` module requires an IAM Role(s) ARN and Name as its input,
which can be created using terraform in a usual way:

```
resource "aws_iam_instance_profile" "credstash-profile" {
  name = "credstash-profile"
  role = "${aws_iam_role.credstash-role.name}"
}
resource "aws_iam_role" "credstash-role" {
  name_prefix  = "credstash-role-"
  assume_role_policy = "${data.aws_iam_policy_document.credstash-role.json}"
}

data "aws_iam_policy_document" "credstash-role" {
  statement {
    sid    = ""
    effect = "Allow"
    principal {
      type = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
    action = ["sts:AssumeRole"]
  }
}
```

The purpose of `credstash-grant` module is to automatically assign
`reader_policy_arn` and/or `writer_policy_arn` to the Role created with above
code as well as create a KMS reader and/or writer grant(s) for that role. This
assures that any EC2 instance assuming this role can use credstash only for
reading and/or writing secrets, but cannot do anything else with neither KMS
Master Key nor DynamoDB table.

Here is how to give read only permission to that role:

```
# lookup credstash remote state
data "terraform_remote_state" "credstash" {
  backend = "s3"
  config {
    region  = "us-west-1"
    bucket  = "remote-tfstate"
    key     = "credstash/terraform.tfstate"
  }
}
module "credstash-grant" {
  source            = "fpco/foundation/aws//modules/credstash-grant"
  kms_key_arn       = "${data.terraform_remote_state.credstash.kms_key_arn}"
  reader_policy_arn = "${data.terraform_remote_state.credstash.reader_policy_arn}"
  roles_count       = 1
  roles_arns        = ["${aws_iam_role.credstash-role.arn}"]
  roles_names       = ["${aws_iam_role.credstash-role.name}"]
}
```

If more than one role is created `roles_count` should be set to the total count
and their respective ARNs and names should be passed in `roles_arns` and
`roles_names` in the same order.

Reading a password from within EC2 User Data is simplified with a few helper snippets:

```
# Install credstash and dependencies:
${data.terraform_remote_state.credstash.install_snippet}
# Retrieve the `high-entropy-password` that was stored before:
${data.terraform_remote_state.credstash.get_cmd} my-secret
```

## Advanced usage

Read/write access can be further narrowed down by
using
[KMS Encryption Context](http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html#encryption-context-authorization).
This is especially useful and recommended when the same KMS Key and DB table are
used amongst different environments (eg. dev, stage, etc.) and/or secrets are
stored for different types of resources/services running on separate EC2
instances (eg. DB credentials, webserver SSL certificates, SSH keys etc.)

Modified example from above:

```
module "credstash-grant" {
  ...
  reader_context = "env=example service=webserver"
  ...
}
```

Using helper snippets to read a secret from within EC2 User Data:

```
${data.terraform_remote_state.credstash.get_cmd} my-super-secret env=example service=webserver
```

**Note**: KMS Encryption Context should not contain sensitive information as it
can be observed in CloudTrail logs.

It is also important that `my-super-secret` was stored using exactly the same
context, otherwise decryption will fail, since encrypted value is cryptographically
bound to the context:

```bash
$ credstash put my-super-secret actual-value env=example service=webserver
my-super-secret has been stored
```

This whole example will ensure that EC2 instance assuming `credstash-role` will
only be able to read from the `credstash-store` and decrypt only values that
where encrypted exactly with `env=example service=webserver` context.

For example, an attempt to retrieve credentials with some other or no context at all
would result in a permission error:

```
${data.terraform_remote_state.credstash.get_cmd} my-other-secret env=production service=webserver
${data.terraform_remote_state.credstash.get_cmd} my-secret
```

Naturally, permission error would occur upon an attempt to write anything to the
store as well, since writer policy was not assigned to the role:

```
${data.terraform_remote_state.credstash.put_cmd} some-secret some-value
```

Despite that above examples do not cover putting secrets in the store with
credstash in much detail, it is very similar to how secrets are read from the
store.

## Example Use Cases

### Dynamic configuration files

If your application supports periodic configuration file reloading, it is
possible to setup a cron job that will be periodically pulling new value with
credstash. This approach allows an admin to store modified configuration file
with credstash at any time and watch all EC2 instances running the application to
automatically update their configuration.

Below is not a very practical example, but it is good enough for conveying the idea. When
using `user_data` to deploy an instance, create this cron job and you'll be able
to update your nginx configuration without having to login remotely or use some
other provisioning tools:

```
# Create a cron job for pulling dynamic config
cat <<EOF > /etc/nginx/dyn-conf-cronjob.sh
#!/bin/bash
${credstash_get_cmd} nginx-default 2>/dev/null >/etc/nginx/sites-available/default
service nginx reload
EOF
chmod a+x /etc/nginx/dyn-conf-cronjob.sh
TMP_CRON=$(mktemp -t "dyn-config-cron-job-XXXXXX.txt")
crontab -l > $TMP_CRON
echo "*/5 * * * * /etc/nginx/dyn-conf-cronjob.sh" >> $TMP_CRON
crontab $TMP_CRON
rm $TMP_CRON
```

This approach is especially useful with terraform and auto scaling
groups. Consider this fact, when using ASGs your instances can come and go at
any time, and the only way to make sure they do have new configuration is either
update launch configuration and restart all running instances, or pull that
configuration from some storage that is always available, like an S3 bucket,
some database or a private repository. This makes credstash a perfect solution
for this sort of setup, particularly if configuration file contains sensitive
information.

### Automatic creation and deployment of TLS certificates

There are a few applications out there that use TLS certificates for mutual
client/server authentication. In this kind of scenario we need to generate six
files:

* CA (Certificate Authority) private key and certificate
* Server TLS key and certificate signed by the above CA
* And a client TLS key and certificate also signed by the above CA

Creating certificates with `openssl` is way to cumbersome and is simply an
overkill for such an isolated environment. A better tool that can help
is [certstrap](https://github.com/square/certstrap). We will use `openssl`
though to convert private keys to PKCS8 form, since that is what applications
tend to support.

Here is a slightly simplified example. Create a `template_file` and a
`null_resource` that will be running rendered script locally:

`main.tf`:
```
data "template_file" "certstrap" {
  template = "${file("${path.module}/data/certs.tpl.sh")}"
  vars {
    domain_name       = "${var.domain_name}" # Server domain name
    depot_path        = "${var.certstrap_depot_path}" # Local path for certificates
    ca_common_name    = "${var.certstrap_ca_common_name}" # CA name
    credstash_put_cmd = "${var.credstash_put_cmd}"
    server_context    = "env=server"
    client_context    = "env=client"
  }
}

resource "null_resource" "certstrap" {
  provisioner "local-exec" {
    command = "${data.template_file.certstrap.rendered}"
  }
}
```

`data/certs.tpl.sh`:
```bash
#!/bin/bash
set -x

CLIENT_NAME="client"
DEPOT_PATH="${depot_path}"
# Remove any old certificate
rm -f "$DEPOT_PATH"

## Create CA SSL Key and Certificate
certstrap --depot-path "$DEPOT_PATH" init --common-name "${ca_common_name}" --passphrase "${ca_passphrase}"

## Create Server SSL Key and Certificate
certstrap --depot-path "$DEPOT_PATH" request-cert --domain "${domain_name}" --passphrase ""
certstrap --depot-path "$DEPOT_PATH" sign "${domain_name}" --CA "${ca_common_name}" --passphrase "${ca_passphrase}"

# Convert Server Key into PKCS8 format.
openssl pkcs8 -topk8 -inform PEM -outform PEM -nocrypt -in "$DEPOT_PATH/${domain_name}.key" -out "$DEPOT_PATH/${domain_name}.pkcs8.key"

## Create Client SSL Key and Certificate
certstrap --depot-path "$DEPOT_PATH" request-cert --domain "$CLIENT_NAME" --passphrase ""
certstrap --depot-path "$DEPOT_PATH" sign "$CLIENT_NAME" --CA "${ca_common_name}" --passphrase "${ca_passphrase}"

# Convert Client Key into PKCS8 format.
openssl pkcs8 -topk8 -inform PEM -outform PEM -nocrypt -in "$DEPOT_PATH/$CLIENT_NAME.key" -out "$DEPOT_PATH/$CLIENT_NAME.pkcs8.key"

# Store all certificates an keys with credstash
${credstash_put_cmd} -a ca_key "@$DEPOT_PATH/${ca_common_name}.key"
${credstash_put_cmd} -a server_ca_cert "@$DEPOT_PATH/${ca_common_name}.crt" ${server_context}
${credstash_put_cmd} -a server_cert "@$DEPOT_PATH/${domain_name}.crt" ${server_context}
${credstash_put_cmd} -a server_key "@$DEPOT_PATH/${domain_name}.pkcs8.key" ${server_context}
${credstash_put_cmd} -a client_ca_cert "@$DEPOT_PATH/${ca_common_name}.crt" ${client_context}
${credstash_put_cmd} -a client_cert "@$DEPOT_PATH/$CLIENT_NAME.crt" ${client_context}
${credstash_put_cmd} -a client_key "@$DEPOT_PATH/$CLIENT_NAME.pkcs8.key" ${client_context}
```

We also create separate roles for the server and for the clients, furthermore we
create separate grants with different contexts for server and client roles,
which has an effect of disabling access to server certificates for clients, as
well as access to client certificate for servers. This isolation is not strictly
required, but certainly is a good idea security wise.

`main.tf`:
```
resource "aws_iam_role" "server-role" {
  name_prefix  = "server-role-"
  assume_role_policy = "${data.aws_iam_policy_document.server-role.json}"
  ...
}
module "server-grant" {
  source            = "fpco/foundation/aws//modules/credstash-grant"
  kms_key_arn       = "${data.terraform_remote_state.credstash.kms_key_arn}"
  reader_policy_arn = "${data.terraform_remote_state.credstash.reader_policy_arn}"
  reader_context    = "env=server"
  roles_count       = 1
  roles_arns        = ["${aws_iam_role.server-role.arn}"]
  roles_names       = ["${aws_iam_role.server-role.name}"]
}

resource "aws_iam_role" "client-role" {
  name_prefix  = "client-role-"
  assume_role_policy = "${data.aws_iam_policy_document.client-role.json}"
  ...
}
module "client-grant" {
  source            = "fpco/foundation/aws//modules/credstash-grant"
  kms_key_arn       = "${data.terraform_remote_state.credstash.kms_key_arn}"
  reader_policy_arn = "${data.terraform_remote_state.credstash.reader_policy_arn}"
  reader_context    = "env=client"
  roles_count       = 1
  roles_arns        = ["${aws_iam_role.client-role.arn}"]
  roles_names       = ["${aws_iam_role.client-role.name}"]
}
```


While deploying with terraform in such a setup, all of the certificates will be
locally created by certstrap, then encrypted and pushed to DynamoDB by
credstash. Inside the user data you can now easily write commands that will
retrieve appropriate certificates.

On servers:

```
${credstash_get_cmd} -n server_ca_cert env=server > /etc/application/ssl/ca.crt
${credstash_get_cmd} -n server_cert env=server > /etc/application/ssl/server.crt
${credstash_get_cmd} -n server_key env=server > /etc/application/ssl/server.key
```

On clients:

```
${credstash_get_cmd} -n client_ca_cert env=client > /etc/application/ssl/ca.crt
${credstash_get_cmd} -n client_cert env=client > /etc/application/ssl/client.crt
${credstash_get_cmd} -n client_key env=client > /etc/application/ssl/client.key
```

And boom, you clients can talk to you servers since they have certificates
signed by the same CA. Worth noting that access is restricted on need to know
basis, for example CA private key should only be available to an administrator,
thus is not accessible by neither servers nor clients, since it was encrypted
with no context at all.
