provider "aws" {
  region = "ap-northeast-1"
}

data "aws_availability_zones" "azs" {}

module "vpc" {
  source               = "fpco/foundation/aws//modules/vpc-scenario-2"
  cidr                 = "192.168.0.0/16"
  public_subnet_cidrs  = ["192.168.0.0/24", "192.168.1.0/24"]
  private_subnet_cidrs = ["192.168.100.0/24", "192.168.101.0/24"]
  azs                  = data.aws_availability_zones.azs.names
  name_prefix          = "ebs-test"
  region               = "ap-northeast-1"
}

module "ubuntu" {
  source = "fpco/foundation/aws//modules/ami-ubuntu"
}

resource "aws_security_group" "ssh" {
  vpc_id = module.vpc.vpc_id
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

module "tester" {
  source             = "../../modules/single-node-asg"
  name_prefix        = "ebs"
  name_suffix        = "test"
  key_name           = "tokyo"
  ami                = module.ubuntu.id
  instance_type      = "t2.micro"
  subnet_id          = module.vpc.public_subnet_ids[0]
  security_group_ids = [aws_security_group.ssh.id]
  region             = "ap-northeast-1"
  compatible_with_single_volume = false
  data_volumes       = [{ name = "a", device = "/dev/xvdm", size = 50 }, { name = "b", device = "/dev/xvdn" }]
}
