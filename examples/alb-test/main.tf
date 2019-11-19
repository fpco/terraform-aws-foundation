provider "aws" {
  region = "us-west-2"
}

data "aws_availability_zones" "azs" {
  state = "available"
}

module "vpc" {
  source              = "fpco/foundation/aws//modules/vpc-scenario-1"
  cidr                = "10.0.0.0/16"
  public_subnet_cidrs = ["10.0.0.0/24", "10.0.1.0/24", "10.0.2.0/24"]
  region              = "us-west-2"
  azs                 = data.aws_availability_zones.azs.names
  name_prefix         = "alb-poc"
}

module "ubuntu" {
  source  = "fpco/foundation/aws//modules/ami-ubuntu"
  release = "18.04"
}

module "asg" {
  source                = "fpco/foundation/aws//modules/asg"
  azs                   = []
  key_name              = "shida-west-2"
  subnet_ids            = [module.vpc.public_subnet_ids[0], module.vpc.public_subnet_ids[2]]
  name_prefix           = "alb-poc"
  min_nodes             = 2
  ami                   = module.ubuntu.id
  max_nodes             = 2
  security_group_ids    = [aws_security_group.sg-asg.id]
  alb_target_group_arns = [module.forwarder.target_group_arn]
  user_data             = <<EOF
#!/bin/bash -
apt update
apt install python3
wget -O web.py https://gist.githubusercontent.com/Magicloud/55831649b38c3a042d01543f84c53a60/raw/184392d46b2f328c5277f748ce59b4b800365b03/http.py
chmod a+x web.py
./web.py &
EOF
}

resource "aws_security_group" "sg-asg" {
  vpc_id = module.vpc.vpc_id
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["10.0.0.0/24"]
  }
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

module "alb" {
  source            = "../../modules/alb"
  name_prefix       = "alb-poc"
  internal          = false
  subnet_ids        = [module.vpc.public_subnet_ids[0], module.vpc.public_subnet_ids[2]]
  vpc_id            = module.vpc.vpc_id
}

module "forwarder" {
  source = "../../modules/alb-default-forward"
  lb_arn = module.alb.lb_arn
  lb_port = 80
  name_prefix = "alb-poc"
  protocol = "HTTP"
  service_port = 80
  vpc_id = module.vpc.vpc_id
}

resource "aws_security_group_rule" "allow_http" {
  type            = "ingress"
  from_port       = 80
  to_port         = 80
  protocol        = "tcp"
  cidr_blocks = ["0.0.0.0/0"]
  security_group_id = module.alb.security_group_id
}

resource "aws_security_group_rule" "open" {
  type            = "egress"
  from_port       = 0
  to_port         = 0
  protocol        = "-1"
  cidr_blocks = ["0.0.0.0/0"]
  security_group_id = module.alb.security_group_id
}

output "test_address" {
  value = module.alb.lb_dns_name
}
