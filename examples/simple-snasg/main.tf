provider "aws" {
  version = "~> 2.18"
  region  = "us-west-2"
}

data "aws_availability_zones" "available" {}

locals {
  region               = "us-west-2"
  name_prefix          = "foundation-snasg-example"
  vpc_cidr             = "10.4.0.0/16"
  key_name             = "${local.name_prefix}-key"
  public_subnets_cidrs = cidrsubnets(local.vpc_cidr, 8, 8)
}

module "vpc" {
  source  = "fpco/foundation/aws//modules/vpc-scenario-1"
  version = "0.9.15"

  name_prefix         = local.name_prefix
  region              = local.region
  cidr                = local.vpc_cidr
  public_subnet_cidrs = local.public_subnets_cidrs
  azs                 = slice(data.aws_availability_zones.available.names,0,2)
}

module "asg-sg" {
  source      = "fpco/foundation/aws//modules/security-group-base"
  version     = "0.9.15"
  name        = "${local.name_prefix}-asg-sg"
  description = "SG for example SNASG"
  vpc_id      = module.vpc.vpc_id
}

module "ami" {
  source  = "fpco/foundation/aws//modules/ami-centos"
  version = "0.9.15"
  release = 7
}

resource "aws_key_pair" "main" {
  key_name   = local.key_name
  public_key = file("id_rsa.pub")
}

module "snasg" {
  # source  = "fpco/foundation/aws//modules/single-node-asg"
  # version = "0.9.15"
  source = "../../modules/single-node-asg"

  region      = local.region
  subnet_id   = module.vpc.public_subnet_ids[0]

  name_prefix = "${local.name_prefix}-asg"
  name_suffix = ""

  key_name      = aws_key_pair.main.key_name
  ami           = module.ami.id
  instance_type = "t2.micro"
  public_ip     = false

  security_group_ids = [module.asg-sg.id]

  init_prefix = ""

  alb_target_group_arns = [module.forwarder.target_group_arn]
}

module "alb" {
  source  = "fpco/foundation/aws//modules/alb"
  version = "0.9.15"

  vpc_id      = module.vpc.vpc_id
  name_prefix = "${local.name_prefix}-alb"
  subnet_ids  = module.vpc.public_subnet_ids
  internal    = false
}

module "forwarder" {
  source  = "fpco/foundation/aws//modules/alb-default-forward"
  version = "0.9.15"

  lb_arn         = module.alb.lb_arn
  lb_port        = 80
  name_prefix    = "${local.name_prefix}-http"
  protocol       = "HTTP"
  service_port   = 80
  vpc_id         = module.vpc.vpc_id
}
