## Ingress Rules for Consul Leaders

Set of security group ingress rules for use with a cluster of consul leaders.
These are the ingress rules for leaders connecting to other leaders.

See `consul-agents-sg` for Agents, or `consul-leaders-wan-sg` for the
cross-WAN leader communication.

 ### Example

```
# Security Group for nomad workers
module "leaders-sg" {
  source      = "../modules/security-group-base"
  name        = "${var.name}-leaders"
  description = "security group for consul leader instances in the private subnet"
  vpc_id      = "${module.vpc.vpc_id}"
}

module "leaders-consul-leader-rules" {
  source            = "../modules/consul-leader-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.leaders-sg.id}"
}

...

module "my-cluster" {
  source             = "../modules/asg"
  ...
  security_group_ids = ["${module.leaders-sg.id}"]
}
```
