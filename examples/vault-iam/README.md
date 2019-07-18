# Vault IAM user

When setting up vault with AWS as it's secret engine, you need to have
AWS secret and access keys for an IAM user with relevant
permission. This example creates IAM user named "vault_user" and
appropriate policy for it.

In order for this example to create keys, you need to modify
`variables.tf` appropriately. Also make sure to change the resource
arn in the policy document in `main.tf` file.

