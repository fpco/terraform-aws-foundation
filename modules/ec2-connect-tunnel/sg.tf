module "tunnel-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-sg"
  description = "SG for the tunnel ASG"
  vpc_id      = var.vpc_id
  extra_tags  = var.extra_tags
}

module "ssh-port-sg-rule" {
  source            = "../single-port-sg"
  security_group_id = module.tunnel-sg.id
  cidr_blocks       = ["0.0.0.0/0"]
  port              = 22
  description       = "SSH from anywhere"
}

# security group rule to open egress (outbound from nodes)
module "allow-open-egress" {
  source            = "../open-egress-sg"
  security_group_id = module.tunnel-sg.id
}
