# Credstash

## Motivation

During automatic deployment of EC2 instances on AWS a very commom question is
what is the best way to deliver sensitive information over to those EC2
instances.  There are numerous possible solutions such as placing it into
user-data initialization script or simply SFTPing them onto the
instance. Although those perfectly viable solutions there are well-known
drawbacks with those approaches, for example size limitations on the former and
necessity to open SSH port for the latter. There are also comprehensive
solutions, such as Consul, that can do a lot more than just deliver credentials,
which sometimes can be an overkill for such a simple issue.

## Introduction

There is a way to solve this by utilizing resources provided only by AWS and a
very cool tool [credstash](https://github.com/fugue/credstash). There is a nice
guide on how to use that tool and description on how it works, but the basic
idea behind credstash is it stores KV data in DynamoDB while encrypting it using
a KMS key and only a person or resource that has read access to the DynamoDB
table and a permission to use that KMS key can access that datya through a very
simple cli. In the most simple the most common case this would look like:

On your laptop:

```bash
$ credstash put my-secret high-entropy-password
my-secret has been stored
```

On the EC2 instance:

```bash
$ credstash get my-secret
high-entropy-password
```


Boom, you got your password across the internet in a totally secure fasion using
nothing but AWS services. So here is what happend in the above example: during a
`put` operation `high-entropy-password` was encrypted using KMS master key with
a default alias `alias/credstash` as a basis for the encryption and stored in a
default DynamoDB table `credstash-store`. How encryption works with KMS is not
the topic for this discussion, but what is important, is that without having
direct access or an explicit grant allowing to use that key, it is impossible to
read the encrypted value. During `get` operation process is inverted and
password is retrieved on the EC2 instance.

## Initial Setup

There are a few custom cli options available with `credstash`, such as usage of
versioning, contexts and ability to specify a custom name for a DB table name
and KMS key alias, but what is of current interest to us the latter two.

Naturally, prior to using credstash, a table and a key have to be created. Going
through credstash documentation will reveal that a DynamoDB table with a name
`credstash-store` can be created by running `credstash setup`, while KMS key has
to be created manually. Well that's no fun, we ought to be able to automate the
whole process. Ability to use a custom name for the table would be nice addition
as well. This terraform
module
[credstash-setup](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/credstash-setup) will
will do just that, thus taking care of the initial setup for us. Remember, we
only need to do it once and make sure not to run `terraform destroy`, unless you
really want your secret data to be permanently deleted.

Although it is not required, I would recommend
using
[terraform's remote state](https://www.terraform.io/docs/state/remote.html)
feature in order to later simplify getting the values for resources that will be
created by the setup in this section. We even have a module that can help you
setup
[s3-remote-state](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/s3-remote-state).

Once applied, below configuration will create DynamoDB table `my-secret-store`, KMS key with
`alias/my-kms-key` and store the output

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
  source        = "github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/credstash-setup"
  kms_key_name  = "my-kms-key"
  db_table_name = "my-secret-store"
}
output "kms_key_arn" {
  value = "${module.credstash.kms_key_arn}"
}
output "db_table_arn" {
  value = "${module.credstash.db_table_arn}"
}
output "db_table_name" {
  value = "${module.credstash.db_table_name}"
}
```

Now let's go ahead and create all credstash related resources. You should see
something along the lines:

```bash
$ terraform apply
...

Outputs:

db_table_name = credential-store
get_cmd = /usr/local/bin/credstash -r us-east-1 -t credential-store get
install_snippet = { apt-get update;
  apt-get install -y build-essential libssl-dev libffi-dev python-dev python-pip;
  pip install --upgrade pip;
  pip install credstash; }

kms_key_alias = alias/credstash
put_cmd = /usr/local/bin/credstash -r us-east-1 -t credential-store put -k alias/credstash
reader_policy_arn = arn:aws:iam::123456789012:policy/personal-credstash-reader
writer_policy_arn = arn:aws:iam::123456789012:policy/personal-credstash-writer
```

Now we can verify locally if the setup worked. Helper snippets are targeted at Ubuntu
based systems, but can be easily adopted to any other operating system.

Let's install credstash, store a test value and pull it out from AWS DynamoDB aterwards:

```bash
$ sudo -H bash -c "$(terraform output install_snippet)"
...
$ $(terraform output put_cmd) test-key test-value
test-key has been stored
$ $(terraform output get_cmd) test-key
test-value
```

We can also set a new value for the key, while auto incrementing its version, by
setting `-a` flag:

```
$ $(terraform output put_cmd) -a test-key new-test-value2
test-key has been stored
$ $(terraform output get_cmd) test-key
new-test-value2
```

There are few other useful features that credstash has, which don't have helper
snippets like `get_cmd` and `put_cmd`, since they are less likely to be used in
automated scripts, but can still be easily constructed using terraform
outputs. Worth noting, that all previous value versions for the key are still
available, unless deleted:

```bash
$ credstash -r us-east-1 -t credential-store get test-key -v 0000000000000000000
test-value
$ credstash -r us-east-1 -t credential-store list
test-key -- version 0000000000000000000
test-key -- version 0000000000000000001
$ credstash -r us-east-1 -t credential-store delete test-key
Deleting test-key -- version 0000000000000000000
Deleting test-key -- version 0000000000000000001
```

Now let's prepare for the next section and store username and password for our website:

```bash
$ $(terraform output put_cmd) nginx-username admin
nginx-username has been stored
$ $(terraform output put_cmd) nginx-password foobar
nginx-password has been stored
```

## Deploy EC2 with credstash

Using credstash directly is extremely simple, but setting everything up for it
too work on EC2 instance can be a bit daunting, so this is what this section
and
[credstash terraform module](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/credstash) are
about.

For the sake of example we will deploy an EC2 instance running nginx with
BasicAuthentication. We will use credstash to retrieve previously stored
credentials to that EC2 instance.


Because `t2.nano` is a very "powerful" instance type we'll give it some time and then
verify that our deployment worked as expected:

```
$ curl -s http://$(terraform output instance_ip) | grep title
<head><title>401 Authorization Required</title></head>
$ curl -s http://admin:foobar@$(terraform output instance_ip) | grep title
<title>Welcome to nginx!</title>
```

Possibilities aren't limitless, but this approach solves the common issues with
trensfering sensitive data over to AWS EC2 instances:

For User Data:
* size limitation of 14KiB is no longer a limitation, considering that I was
  able to store files around 200KiB using credstash. So it will work just fine
  with SSL certificates, medium size configuration files, etc. And, if
  necessary, larger files could be split among many keys, but that, I think,
  would already be an abuse of credstash.
* Feeding sensitive information using user data is considered by some to be less
  secure, because it is available to any user with access to EC2 instance
  throughout its lifetime. There is some sense in this, but if a malicious
  idividual was able to get access to the machine, it is already a tragedy, not
  even considering further possibility of privilege escalation. Despite this
  fact, with credstash, sensitive data can be written to storage with root only
  access or piped to an application directly completely bypassing local
  filesystem. You might ask, credstash doesn't need root access to get all of
  the credentials again from the secret storage, so situation is even worse then
  before. This problem can be easily aliviated by manually removing a KMS grant
  and/or detaching the `reader_policy` from the IAM Role after an instance was
  completely deployed. Narrowing the access with use of contexts could also be
  of help whenever credstash store is the main storage for sensitive
  information.
