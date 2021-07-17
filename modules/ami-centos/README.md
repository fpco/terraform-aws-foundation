## Centos AMI lookup helper

This module is a simple helper that looks up the current AMI ID for a given
release of Centos. Use it like:

```
module "centos-7-ami" {
  source         = "../../modules/ami-centos"
  release        = 7
  image_provider = "CentOS"
}
```

Or:

```
module "centos-8-ami" {
  source         = "../../modules/ami-centos"
  release        = 8
  image_provider = "AWS"
}
```

To use the AMI on EC2, reference it by ID like this: `${module.centos-7-ami.id}`

The module will filter the AMI by the following criteria:

* provided by Centos.org or AWS
* the most recent release
* hvm-type AMIs
* amd64
* release

If you deploy an instance with this AMI, and later do a `terraform plan`, the
most recent AMI will be looked up, and that may change the module output, and
then the AMI for the instance. You can use `ignore_changes` or `-target`, to
work around that situation, or take that as your reminder to replace the
instance with a more recent release of the upstream AMI.
