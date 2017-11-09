data "aws_availability_zones" "available" {}

data "aws_ami" "ubuntu-trusty" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-trusty-14.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  #          GovCloud        Standard AWS
  owners = ["513442679011", "099720109477"] # Canonical
}

data "aws_ami" "ubuntu-xenial" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  #          GovCloud        Standard AWS
  owners = ["513442679011", "099720109477"] # Canonical
}
