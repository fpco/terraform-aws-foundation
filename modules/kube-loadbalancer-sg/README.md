## Kube Load Balancer Security Group

Define a security group for kube load balancer
provisioning Kube Load Balancer example:
```
module "sg-lb" {
  source      = "../../modules/kube-loadbalancer-sg"
  name_prefix = "${var.name}"
  name_suffix = "kube-loadbalancer"
  vpc_id      = "${module.vpc.vpc_id}"
  vpc_cidr    = "${var.vpc_cidr}"
}
```
