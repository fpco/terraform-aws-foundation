locals {
  cidr                 = "192.168.0.0/16"
  private_subnet_cidrs = ["192.168.100.0/24", "192.168.101.0/24"]
  public_subnet_cidrs  = ["192.168.0.0/24", "192.168.1.0/24"]
}


data "aws_vpc" "vpc" {
  cidr_block = local.cidr
}

data "aws_subnet" "public" {
  count = length(local.public_subnet_cidrs)
  cidr_block = local.public_subnet_cidrs[count.index]
}

module "centos" {
  source  = "fpco/foundation/aws//modules/ami-centos"
  release = 7
}

resource "aws_security_group" "asg" {
  vpc_id = data.aws_vpc.vpc.id

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

module "s3" {
  source = "../../modules/s3-enc"
  bucket_name = "shida-test"
  reader_role_name = module.instance_profile.iam_role_name
  writer_role_name = module.instance_profile.iam_role_name
}

module "instance_profile" {
  source      = "fpco/foundation/aws//modules/iam-instance-profile"
  name_prefix = "shida-test"
}

resource "aws_instance" "master" {
  ami                         = module.centos.id
  associate_public_ip_address = true
  iam_instance_profile        = module.instance_profile.iam_profile_id
  instance_type               = "t2.micro"
  key_name                    = "shida-tokyo"
  subnet_id                   = data.aws_subnet.public[0].id
  vpc_security_group_ids      = [aws_security_group.asg.id]
  tags = {
    "ledger_initeq_net-owner" = "shida",
  }
  user_data = <<EOF
#!/bin/bash
yum install -y python3-pip
pip3 install awscli
EOF
}

output "master" {
  value = aws_instance.master.public_ip
}
