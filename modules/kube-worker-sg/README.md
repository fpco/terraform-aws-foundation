## Kube Worker Security Group

This module defines a security group for worker nodes in a kubernetes cluster.
The module adds security group rules to allow access to the following:

* access to all ports on the worker nodes (usually limited to nodes in the VPC)
* SSH on the kube worker nodes
* open egress for the worker nodes


### How to use this module

```terraform
module "kube-worker-sg" {
  source           = "fpco/foundation/aws//modules/kube-worker-sg"
  version          = "..."
  name_prefix      = "${var.name}"
  vpc_id           = "${module.vpc.vpc_id}"
  cidr_blocks_ssh  = ["${var.corporate_net}"]

  # these networks have access to all ports on the kube worker nodes
  cidr_blocks_open_ingress = ["${var.vpc_cidr}"]
}
```
