module "k8s-controller" {
  source          = "../asg"
  ami             = var.ami
  name_prefix     = var.name_prefix
  name_suffix     = var.name_suffix
  azs             = [var.az]
  key_name        = var.key_name
  max_nodes       = "1"
  min_nodes       = "1"
  instance_type   = var.instance_type
  placement_group = var.placement_group
  subnet_ids      = [var.subnet_id]
  elb_names       = var.elb_names
  extra_tags      = var.extra_tags
  iam_profile     = var.iam_profile
  user_data       = var.user_data

  security_group_ids = var.security_group_ids
}

