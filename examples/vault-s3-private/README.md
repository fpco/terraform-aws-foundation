# Example showing Vault and IAM Integration

This example creates a private s3 bucket resources. It then uses vault
to create keys which only has access to those s3 buckets. The example
code will create an IAM role with access to that bucket and will also
configure vault so that we can dynamically generate credentials for
accessing that bucket.

## Requirements

These are the required things for this example:

* A running vault server. If you just want to experiment with this,
  run a development server using:

``` shellsession
vault server -dev
```

* The AWS access and secret keys for an IAM user which the AWS Secret
  Backend for Vault will use for issuing new credentials. If you don't
  have any, you can create one using [vault-iam
  example](../vault-iam). You need to put the access keys in
  [variables.tf](./variables.tf)


## Environment creation and deployment

``` shellsession
$ make init
$ make plan
$ make apply
module.vault_aws_backend.vault_aws_secret_backend.aws: Creating...
module.vault_aws_backend.vault_aws_secret_backend.aws: Creation complete after 0s [id=fpco/aws/dev/vault]
aws_iam_role.vault_bucket_role: Creating...
aws_s3_bucket.vault-test-bucket: Creating...
aws_iam_role.vault_bucket_role: Still creating... [10s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [10s elapsed]
aws_iam_role.vault_bucket_role: Still creating... [20s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [20s elapsed]
aws_iam_role.vault_bucket_role: Creation complete after 22s [id=bucket_access_role]
module.vault_aws_backend.vault_aws_secret_backend_role.aws_role: Creating...
module.vault_aws_backend.vault_aws_secret_backend_role.aws_role: Creation complete after 0s [id=fpco/aws/dev/vault/roles/s3_app_user]
aws_s3_bucket.vault-test-bucket: Still creating... [30s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [40s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [50s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [1m0s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [1m10s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [1m20s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [1m30s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [1m40s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [1m50s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [2m0s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [2m10s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [2m20s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [2m30s elapsed]
aws_s3_bucket.vault-test-bucket: Still creating... [2m40s elapsed]
aws_s3_bucket.vault-test-bucket: Creation complete after 2m48s [id=vault-fpco-test-bucket]
aws_iam_role_policy.vault_bucket_policy: Creating...
aws_iam_role_policy.vault_bucket_policy: Still creating... [10s elapsed]
aws_iam_role_policy.vault_bucket_policy: Still creating... [20s elapsed]
aws_iam_role_policy.vault_bucket_policy: Creation complete after 24s [id=bucket_access_role:bucket-policy]

Apply complete! Resources: 5 added, 0 changed, 0 destroyed.

The state of your infrastructure has been saved to the path
below. This state is required to modify and destroy your
infrastructure, so keep it safe. To inspect the complete state
use the `terraform show` command.

State path: terraform.tfstate
```

## Testing

Make sure you are already authorized with the vault server. If not,
use `vault login` to do it. And then, you can dynamically create AWS
credentials for accessing the s3 bucket you created:

``` shellsession
$ vault read fpco/aws/dev/vault/creds/s3_app_user
Key                Value
---                -----
lease_id           fpco/aws/prod/vault/creds/s3_app_user/eJcLUNbpTNRFpLoTL9mEW76p
lease_duration     14m59s
lease_renewable    false
access_key         xxx
secret_key         xxx
security_token     xxx
```

Now let's try to see all the files in our bucket:

``` shellsession
$ env AWS_ACCESS_KEY_ID=xxx AWS_SECRET_ACCESS_KEY=xxx AWS_SESSION_TOKEN=xxx aws s3 ls s3://s3-vault-demo-dev-bucket
```

It gives you no output since there are no files. But the command
works, which confirms us that the generated credentials are working as
expected.

Now let's try to do something for which you don't have access with the
same credentials:

``` shellsession
$ env AWS_ACCESS_KEY_ID=xxxx AWS_SECRET_ACCESS_KEY=xxxx AWS_SESSION_TOKEN=xxx aws ec2 describe-instances --region="us-east-2"
An error occurred (UnauthorizedOperation) when calling the DescribeInstances operation: You are not authorized to perform this operation.
```

That doesn't work, which is expected. Let's try to see if we can
access files of some other buckets which is present:

``` shellsession
$ env AWS_ACCESS_KEY_ID=xxx AWS_SECRET_ACCESS_KEY=xxx AWS_SESSION_TOKEN=xxx aws s3 ls s3://some-other-existing-bucket
An error occurred (AccessDenied) when calling the ListObjects operation: Access Denied
```

## Destruction

``` shellsession
$ make destroy
$ make clean
```

## Notes

- This example was last tested with `Terraform v0.12.3`
- This example assumes AWS credentials setup with access to the **us-east-2** region.
