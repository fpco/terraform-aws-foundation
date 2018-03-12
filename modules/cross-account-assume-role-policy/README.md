## Cross-Account Assume Role Policy

Creates an IAM policy that allows attached entities to assume given role(s)
in given account(s).

Either `account_ids` and `role_name` can be provided, OR `account_id` and
`role_names`. Other combinations will not work correctly due to limitations
in Terraform.
