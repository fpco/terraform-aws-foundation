provider "aws" {
  region = var.region
}

data "aws_availability_zones" "available" {
}

module "vpc" {
  source      = "../../modules/vpc-scenario-1"
  name_prefix = var.name
  region      = var.region
  cidr        = var.vpc_cidr
  azs         = [data.aws_availability_zones.available.names[0]]
  dns_servers = aws_directory_service_directory.main.dns_ip_addresses
  extra_tags  = var.extra_tags

  public_subnet_cidrs = var.public_subnet_cidrs
}

resource "aws_key_pair" "main" {
  key_name   = var.name
  public_key = file(var.ssh_pubkey)
}

module "web-sg" {
  source      = "../../modules/security-group-base"
  description = "For my-web-app instances in ${var.name}"
  name        = "${var.name}-web"
  vpc_id      = module.vpc.vpc_id
}

# shared security group, open egress (outbound from nodes)
module "web-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = module.web-sg.id
}

resource "aws_security_group_rule" "ssh" {
  type              = "ingress"
  from_port         = 22
  to_port           = 22
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = module.web-sg.id
}

data "template_file" "init" {
  template = file("${path.module}/bootstrap.linux.txt")

  vars = {
    ad_password = var.active_directory_password
    ad_domain   = local.domain
  }
}

data "template_cloudinit_config" "config" {
  # Main cloud-config configuration file.
  part {
    filename     = "init.cfg"
    content_type = "text/cloud-config"
    content      = "${data.template_file.init.rendered}"
  }
}


module "web-asg" {
  source             = "../../modules/asg"
  ami                = var.ami_id
  azs                = []
  name_prefix        = "${var.name}-asg"
  instance_type      = "t2.micro"
  max_nodes          = 1
  min_nodes          = 1
  public_ip          = true
  key_name           = aws_key_pair.main.key_name
  subnet_ids         = module.vpc.public_subnet_ids
  security_group_ids = [module.web-sg.id]

  root_volume_type = "gp2"
  root_volume_size = "40"

  user_data = data.template_cloudinit_config.config.rendered
}

module "private-subnets" {
  source      = "../../modules/subnets"
  azs         = slice(data.aws_availability_zones.available.names, 1, 3)
  vpc_id      = module.vpc.vpc_id
  name_prefix = "${var.name}-private"
  cidr_blocks = var.private_subnet_cidrs
  public      = false
  extra_tags  = merge(var.extra_tags, var.private_subnet_extra_tags)
}

module "nat-gateway" {
  source             = "../../modules/nat-gateways"
  vpc_id             = module.vpc.vpc_id
  name_prefix        = var.name
  nat_count          = length(var.public_subnet_cidrs)
  public_subnet_ids  = module.vpc.public_subnet_ids
  private_subnet_ids = module.private-subnets.ids
  extra_tags         = merge(var.extra_tags, var.nat_gateway_extra_tags)
}

resource "aws_directory_service_directory" "main" {
  name     = local.domain
  password = var.active_directory_password
  size     = "Small"
  edition  = "Standard"
  type     = "MicrosoftAD"

  vpc_settings {
    vpc_id     = module.vpc.vpc_id
    subnet_ids = module.private-subnets.ids
  }
}
