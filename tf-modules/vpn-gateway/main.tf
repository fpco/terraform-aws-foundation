/**
 * ## VPN Gateway
 *
 * Create a stand-alone `aws_instance` as a VPN gateway.
 *
 */

resource "aws_key_pair" "vpn-gateway" {
  key_name = "${var.name_prefix}-vpn-gateway-key"
  public_key = "${var.public_key}"
}

data "aws_ami" "ubuntu" {
  most_recent = true
  filter {
    name = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-*"]
  }
  filter {
    name = "root-device-type"
    values = ["ebs"]
  }
  owners = ["099720109477"] # Canonical
}

resource "aws_instance" "vpn-gateway" {
  ami                         = "${data.aws_ami.ubuntu.id}"
  instance_type               = "${var.instance_type}"
  source_dest_check           = false
  key_name                    = "${aws_key_pair.vpn-gateway.key_name}"
  subnet_id                   = "${var.vpc_subnet_id}"
  vpc_security_group_ids      = ["${aws_security_group.vpn-gateway.id}"]
  associate_public_ip_address = true

  connection {
    type = "ssh"
    user = "ubuntu"
    private_key = "${var.private_key}"
  }
  
  provisioner "file" {
    source = "${path.module}/scripts/vpn-gateway"
    destination = "/tmp"
  }

  provisioner "remote-exec" {
    inline = [
      "sudo apt-get update -q",
      "sudo apt-get install -y -q docker docker.io",
      "sudo service docker start",
      "sudo docker build /tmp/vpn-gateway/ -q --tag=${var.vpn_docker_image}",
      "sudo bash -c 'echo docker-image ${var.vpn_docker_image} > ${var.vpn_config_file}'",
      "sudo bash -c 'echo vpc-cidr ${var.vpc_cidr} >> ${var.vpn_config_file}'",
      "sudo bash -c 'echo hostname ${var.vpn_hostname} >> ${var.vpn_config_file}'",
      "sudo bash -c 'echo username \"${var.vpn_username}\" >> ${var.vpn_config_file}'",
      "sudo bash -c 'echo password \"${var.vpn_password}\" >> ${var.vpn_config_file}'",
      "sudo cp /tmp/vpn-gateway/vpn-gateway /etc/init.d/vpn-gateway",
      "sudo chmod +x /etc/init.d/vpn-gateway",
      "sudo update-rc.d vpn-gateway defaults",
      "sudo service vpn-gateway start"
    ]
  }
  
  tags = {
    Name = "${var.name_prefix}-vpn-gateway"
  }
}

resource "aws_security_group" "vpn-gateway" {
  name        = "vpn-gateway-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Security group for vpn gateway (Allow ALL from VPC CIDR)"

  ingress {
    cidr_blocks = ["${var.vpc_cidr}"]
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
  }

  ingress {
    cidr_blocks = ["0.0.0.0/0"]
    from_port   = 0
    to_port     = 0
    protocol    = "icmp"
  }

  ingress {
    cidr_blocks = ["0.0.0.0/0"]
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
  }

  egress {
    cidr_blocks = ["0.0.0.0/0"]
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
  }
}

## Create a route for all traffic to private IP range to be handled by vpn-gateway.
resource "aws_route" "vpc-vpn-route" {
  route_table_id         = "${var.vpc_route_table_id}"
  destination_cidr_block = "${var.vpn_cidr}"
  instance_id            = "${aws_instance.vpn-gateway.id}"
}

## Associate Private Hosted Zone with current VPC.
resource "aws_route53_zone_association" "main" {
  zone_id = "${var.route53_zone_id}"
  vpc_id  = "${var.vpc_id}"
}
