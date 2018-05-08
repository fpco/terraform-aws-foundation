# Remote State on S3

## Purpose of This Module

The purpose of this module is to create a remote backend to store Terraform deployment state in an S3 bucket. The module:

* creates the S3 bucket with versioning enabled
* creates an IAM policy that has full access to the bucket
* works with a specific role you define

## How to Use This Module

The use of this module has two stages. In the first stage we use a wrapper project to create the S3 bucket with appropriate policies. In the second stage we configure and initialize the remote backend. At the end of that process the state will be copied over to the remote backend and be available for other people to use as well.

### Wrapper Project

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

#terraform {
#  backend "s3" {
#    encrypt = "true"
#    bucket  = "fpco-tf-remote-state-test"
#    key     = "mod/remote-state-storage/terraform.tfstate"
#  }
#}

module "s3-remote-state-bucket" {
    source      = "fpco/foundation/aws//modules/s3-remote-state"
    bucket_name = "${var.bucket_name}"
}

output "bucket_name" {
    value = "${module.s3-remote-state-bucket.bucket_id}"
}
```

Create a `terraform.tfvars`, for example:

```hcl
principals="<arn of principal>"
bucket_name="fpco-tf-remote-state-test"
```

Now it's time to create  the `IAM` policies and `S3` bucket using Terraform.

* Initialize the project with `terraform init.` 
* Review the changes Terraform will make with `tf plan --out=tf.out`, and 
* Apply those changes with `tf apply tf.out`.

### Initialize the Remote State S3 Backend

We want to use the newly created S3 bucket to store the Terraform state for this module. There are several steps that need to be taken. Make sure to backup your existing `terraform.tfstate` file before taking these steps:

1. First we have to [initialize the backend][1] and copy over the current state of the backend bucket you just created. Uncomment the following, so that the code in the `main.tf` file looks similar to this:

```hcl
terraform {
  backend "s3" {
    encrypt = "true"
    bucket  = "fpco-tf-remote-state-test"
    key     = "mod/remote-state-storage/terraform.tfstate"
  }
}
```

2. Now run the command `terraform init`.  You will get dialog similar to what you see in what follows. Where you are prompted with ** Enter a value:**, you should enter *yes*.

```hcl
Initializing modules...
- module.s3-remote-state-bucket

Initializing the backend...
Do you want to copy existing state to the new backend?
  Pre-existing state was found while migrating the previous "local" backend to the
  newly configured "s3" backend. No existing state was found in the newly
  configured "s3" backend. Do you want to copy this state to the new "s3"
  backend? Enter "yes" to copy and "no" to start with an empty state.

  Enter a value: yes


Successfully configured the backend "s3"! Terraform will automatically
use this backend unless the backend configuration changes.

Initializing provider plugins...

The following providers do not have any version constraints in configuration,
so the latest version was installed.

To prevent automatic upgrades to new major versions that may contain breaking
changes, it is recommended to add version = "..." constraints to the
corresponding provider blocks in configuration, with the constraint strings
suggested below.

* provider.aws: version = "~> 1.26"

Terraform has been successfully initialized!
```

3. Now that the initial configuration is done and we have copied over the state, we can permanently configure any other wrapper module to make use of the remote state. You can do this by copying the full configuration noted in step 1. This will ensure that anyone else who  will be using the existing configuration found in the S3 backend, once they run `terraform init`.

### Recommendations

* For simplicity, it is generally best to use this module in its own Terraform
  project.
* Define and manage the bulk of your IAM users/policies/groups elsewhere, keep
  this project limited to managing the remote state bucket.

## Other Stuff You Can Do

Review the contents of the bucket with:

```bash
ᐅ aws s3 ls $(tfo bucket_name)
2016-11-22 13:47:55       7860 foobar-remote-state
```

Copy the state to a local file:

```bash
ᐅ aws s3 cp s3://$(tfo bucket_name)/$(tfo bucket_name) foobar.json
download: s3://foobar-remote-state/foobar-remote-state to ./foobar.json
```

Run a diff on the remote and local copies:

```bash
ᐅ diff foobar.json .terraform/terraform.tfstate
```

## Outputs

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

## Deleting the state and buckets

If for any reason you no longer need this state and bucket, you can delete it as follows:

1. re-comment the `terraform` section with the remote state description and rerun `terrform init`. This will prompt with a question whether you want to copy back the remote state to local. Answer *yes*.
2. Manually delete the contents of the `S3` bucket
3. Run `terraform destroy`

[1]: https://www.terraform.io/docs/backends/config.html