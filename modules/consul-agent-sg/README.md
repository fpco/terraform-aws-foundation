## Ingress Rules for Consul Agents

This module creates `aws_security_group_rule` resources, defining an ingress
rule to allow port `8301`, for TCP and UDP each. Use with a security
group on any nodes you wish to use the consul agent on.


### Example

```
# Security Group for nomad workers
module "workers-sg" {
  source      = "../modules/security-group-base"
  name        = "${var.name}-workers"
  description = "security group for worker instances in the private subnet"
  vpc_id      = "${module.vpc.vpc_id}"
}

module "workers-consul-agent-rule" {
  source            = "../modules/consul-agent-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.workers-sg.id}"
}

...

module "my-cluster" {
  source             = "../modules/asg"
  ...
  security_group_ids = ["${module.workers-sg.id}"]
}
```
