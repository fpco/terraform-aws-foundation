## Ubuntu AMI lookup helper

This module is a simple helper that looks up the current AMI ID for a given
release of ubuntu. Use it like:

```
module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
}
```

Or for Trusty:

```
module "ubuntu-trusty-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "14.04"
}
```

To use the AMI on EC2, reference it by ID like this: `${module.ubuntu-xenial-ami.id}`

The module will filter the AMI by the following criteria:

* Canonical's account ARN (for either corporate or govcloud partitions)
* the most recent release
* hvm-type AMIs
* amd64
* the `name` filter

The `name` filter looks like:

```
filter {
  name   = "name"
  values = ["ubuntu/images/hvm-ssd/ubuntu-${var.name_map[var.release]}-${var.release}-amd64-server-*"]
}
```

If you deploy an instance with this AMI, and later do a `terraform plan`, the
most recent AMI will be looked up, and that may change the module output, and
then the AMI for the instance. You can use `ignore_changes` or `-target`, to
work around that situation, or take that as your reminder to replace the
instance with a more recent release of the upstream AMI.
