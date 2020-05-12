module "tunnel-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-sg"
  description = "SG for the tunnel ASG"
  vpc_id      = var.vpc_id
  extra_tags  = var.extra_tags
}

# security group rule to open egress (outbound from nodes)
module "allow-open-egress" {
  source            = "../open-egress-sg"
  security_group_id = module.tunnel-sg.id
}
