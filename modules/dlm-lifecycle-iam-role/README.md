## Data Lifecycle Manager (DLM) lifecycle policy for managing snapshots

This module creates the IAM role and the policy that allows the AWS Data Lifecycle Manager to create snapshots.

### Example how to use

Define variables

```
module "ebs-backup-policy" {
  source = "github.com/fpco/terraform-aws-foundation//modules/dlm-lifecycle-iam"

  iam_role_name = "dlm-lifecycle-role"
}
```
