## Kube Load Balancer Security Group

This module defines a security group for controller load balancers in a
kubernetes cluster. The module adds security group rules to allow access
to the following:

* the kube controller API
* open egress for the load balancer nodes


### How to use this module

```terraform
module "kube-load-balancer-sg" {
  source      = "fpco/foundation/aws//modules/kube-load-balancer-sg"
  name_prefix = "${var.name}"
  vpc_id      = "${module.vpc.vpc_id}"

  # omit this to use the default (public access, eg `0.0.0.0/0`)
  cidr_blocks_api = ["${var.vpc_cidr}", "${var.corporate_network}"]
}
```
