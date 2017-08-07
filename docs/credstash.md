# Credstash

[credstash](https://github.com/fugue/credstash) is a command line tool that
leverages KMS to encrypt/decrypt sensitive information and DynamoDB to store the
data in encrypted form.

## Setup

In order to start using `credstash`, KMS Master Key and DynamoDB table need to
be created. Also, to get access to secrets in the store with credstash, IAM
roles and policies ought to be used.

[credstash-setup](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/credstash-setup) is
the module that can take care of the whole setup and need to be applied only
once. This module creates resources in the current region:

* DynamoDB table with default value: `credstash-store`
* KMS Master Key and an alias for it with default value: `alias/credstash`
* IAM Policy with full access to above KMS Key
* Default IAM Role attached to above policy that can be used for managing that
  key: modify/delete the key, encrypt/decrypt with the key, etc. Optionally
  extra IAM users and roles can be attached to above policy through
  `kms_key_admins` variable.
* Reader and/or Writer IAM Policies that give access to created DynamoDB table.
  Those policies can later be assigned to a role assumed by an EC2 instance that
  will be reading/writing secrets with `credstash`. More on that in the next
  section.


Unless multiple tables and KMS keys are used by credstash within the same AWS
account, most of the variables in the module can be kept with their default
values. Here is an example of `credstash-setup` module usage:

```hcl
module "credstash" {
  source               = "github.com/fpco/fpco-terraform-aws/tf-modules/credstash-setup"
  name_prefix          = "dev"
  create_reader_policy = true # can be ommitted if secrets are writeonly from within EC2
  create_writer_policy = true # can be ommitted if secrets are readonly from within EC2
  kms_key_admins       = ["arn:aws:iam::111111111111:user/developer"]
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

After `credstash` related resources have been setup any user from the list of
`kms_key_admins` can start using the tool.

Here is a simple example on how to use it on the terminal:

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
[credstash-grant](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/credstash-grant) terrform
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
  assume_role_policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "ec2.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
END_POLICY
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
module "credstash-grant" {
  source            = "github.com/fpco/fpco-terraform-aws/tf-modules/credstash-grant"
  kms_key_arn       = "${data.terraform_remote_state.credstash.kms_key_arn}"
  reader_policy_arn = "${data.terraform_remote_state.credstash.reader_policy_arn}"
  roles_count       = 1
  roles_arns        = ["${aws_iam_role.credstash-role.arn}"]
  roles_names       = ["${aws_iam_role.credstash-role.name}"]
}
```

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
context, otherwise decryption will fail, since encrypted value is encryptionally
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
${data.terraform_remote_state.credstash.get_cmd} my-super-secret env=production service=webserver
${data.terraform_remote_state.credstash.get_cmd} my-secret
```

Naturally, permission error would occur upon an attempt to write anything to the
store as well, since writer policy was not assigned to the role:

```
${data.terraform_remote_state.credstash.put_cmd} some-secret some-value
```

Despite that above examples do not cover putting secrets in the store with
credstash in much detail, it is almost the same as doing the reading of secrets.
