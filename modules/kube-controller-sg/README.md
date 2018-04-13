## Kube Controller Security Group

This module defines a security group for controller nodes in a kubernetes
cluster. The module adds security group rules to allow access to the following:

* the kube controller API
* SSH on the kube controller nodes
* etcd on the kube controller nodes
* open egress for the controller nodes


### How to use this module

```terraform
module "kube-controller-sg" {
  source           = "fpco/foundation/aws//modules/kube-controller-sg"
  version          = "..."
  name_prefix      = "${var.name}"
  vpc_id           = "${module.vpc.vpc_id}"
  cidr_blocks_api  = ["${var.vpc_cidr}", "${var.corporate_net}"]
  cidr_blocks_ssh  = ["${var.corporate_net}"]
  cidr_blocks_etcd = ["${var.vpc_cidr}"]
}
```
