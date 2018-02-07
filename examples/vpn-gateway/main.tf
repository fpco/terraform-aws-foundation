provider "aws" {
  access_key = "${var.access_key}"
  region     = "${var.region}"
  secret_key = "${var.secret_key}"
  token      = "${var.token}"
}

module "vpc" {
  source = "../../vpc"

  azs                 = ["${var.vpc_azs}"]
  cidr                = "${var.vpc_cidr}"
  name_prefix         = "${var.name_prefix}"
  public_subnet_cidrs = ["${var.vpc_public_subnet_cidrs}"]
  region              = "${var.region}"
  extra_tags          = {}
}

module "vpn-gateway" {
  source = "../../vpn-gateway"

  name_prefix        = "${var.name_prefix}"
  instance_type      = "t2.nano"
  public_key         = "${file("${var.pub_key_file}")}"
  private_key        = "${file("${var.priv_key_file}")}"
  vpc_id             = "${module.vpc.vpc_id}"
  vpc_cidr           = "${var.vpc_cidr}"
  vpc_subnet_id      = "${module.vpc.public_subnet_ids[0]}"
  vpc_route_table_id = "${module.vpc.public_route_table_id}"
  route53_zone_id    = "${var.route53_zone_id}"
  vpn_cidr           = "${var.vpn_cidr}"
  vpn_username       = "${var.vpn_username}"
  vpn_password       = "${var.vpn_password}"
  vpn_hostname       = "${var.vpn_hostname}"
}


resource "aws_instance" "ec2-test-instance" {
  ami             = "${module.vpn-gateway.ami}"
  instance_type   = "t2.nano"
  subnet_id       = "${module.vpc.public_subnet_ids[1]}"
  security_groups = ["${aws_security_group.ec2-test-sg.id}"]
  key_name        = "${aws_key_pair.ec2-test-key.id}"
  connection {
    type = "ssh"
    user = "ubuntu"
    private_key = "${file("${var.priv_key_file}")}"
  }
  associate_public_ip_address = true

  tags {
    Name = "test-instance"
  }
}

resource "aws_security_group" "ec2-test-sg" {
  name        = "ec2-test-sg"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Allow inbound, SSH, ICMP and everything outbound"

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_key_pair" "ec2-test-key" {
  key_name = "ec2-test-key"
  public_key = "${file("${var.pub_key_file}")}"
}

output "route53_zone_id" {
  value = "${var.route53_zone_id}"
}

output "vpn_gateway_public_ip" {
  value = "${module.vpn-gateway.public_ip}"
}

output "test_instance_public_ip" {
  value = "${aws_instance.ec2-test-instance.public_ip}"
}
