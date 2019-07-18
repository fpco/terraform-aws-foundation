provider "aws" {
  region = "us-west-1"
}

module "ami" {
  source  = "../../modules/ami-centos"
  release = "7"
}

resource "aws_instance" "test" {
  ami             = "${module.ami.id}"
  instance_type   = "t2.micro"
  key_name        = "shida-west-1"
  security_groups = ["ssh_only"]
}

resource "aws_security_group" "ssh_only" {
  name = "ssh_only"
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

output "ip" {
  value = "${aws_instance.test.public_ip}"
}
