# Remote State on S3

This module simplifies the setup of an S3 bucket for Terraform's remote state
storage. The module will:

* create the S3 bucket with versioning enabled
* create an IAM policy that has full access to the bucket
* attach a list of users and/or groups to that policy


## How to use this module

### Minimal Use

Create a new Terraform project and a `main.tf` with the following:

```hcl
variable "bucket_name" {
    description = "the name to give the bucket"
}
variable "principals" {
    default     = ""
    description = "list of IAM user/role ARNs with access to the bucket"
}

provider "aws" { }

module "s3-remote-state-bucket" {
    source      = "../../../../fpco-terraform-aws/tf-modules/s3-remote-state"
    iam_users   = "${var.iam_users}"
    iam_groups  = "${var.iam_groups}"
    bucket_name = "${var.bucket_name}"
}

output "bucket_name" {
    value = "${module.s3-remote-state-bucket.bucket_id}"
}
```

Create a `terraform.tfvars`, for example:

```hcl
principals="example needed here"
bucket_name="foobar-remote-state"
```

Review the changes Terraform will make with `tf plan`, and apply those changes
with `tf apply`.

Configure remote state to use that bucket with:

```
ᐅ tf remote config -backend=s3 -backend-config="bucket=$(tfo bucket_name)" -backend-config="key=$(tfo bucket_name)" -backend-config="encrypt=true" -pull=false
Remote state management enabled
```

Push the existing state to that bucket:

```
ᐅ tf remote push
State successfully pushed!
```


### Recommendations

* For simplicity, it is generally best to use this module in its own Terraform
  project.
* The first time you apply the Terraform code to setup the bucket and its access
  policy, let Terraform store the state locally as a file. Then configure remote
  state to store the state _in the bucket you just created_.
* Define and manage the bulk of your IAM users/policies/groups elsewhere, keep
  this project limited to managing the remote state bucket.


## other stuff you can do

Review the contents of the bucket with:

```
ᐅ aws s3 ls $(tfo bucket_name)
2016-11-22 13:47:55       7860 foobar-remote-state
```

Copy the state to a local file:

```
ᐅ aws s3 cp s3://$(tfo bucket_name)/$(tfo bucket_name) foobar.json
download: s3://foobar-remote-state/foobar-remote-state to ./foobar.json
```

Run a diff on the remote and local copies:

```
ᐅ diff foobar.json .terraform/terraform.tfstate
```


### Outputs

The following are outputs that are worth considering, though only the
`bucket_name` output is necessary for basic operations (the others are helpful
for more advanced use of this module, when exporting outputs to other projects
for example):

```hcl
output "bucket_name" {
    value = "${module.s3-remote-state-bucket.bucket_id}"
}
output "bucket_arn" {
    value = "${module.s3-remote-state-bucket.bucket_arn}"
}
output "region" {
    value = "${module.s3-remote-state-bucket.region}"
}
output "iam_policy_arn" {
    value = "${module.s3-remote-state-bucket.iam_policy_arn}"
}
output "iam_policy_name" {
    value = "${module.s3-remote-state-bucket.iam_policy_name}"
}
output "principals" {
    value = "${module.s3-remote-state-bucket.principals}"
}
```

