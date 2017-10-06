module "ubuntu-ami" {
  source      = "../ami-ubuntu"
  release     = "16.04"
  is_govcloud = "${var.is_govcloud}"
}

module "public-ssh-sg" {
  source              = "../ssh-sg"
  name                = "${var.name_prefix}-ec2-nat-public"
  vpc_id              = "${var.vpc_id}"
  allowed_cidr_blocks = "0.0.0.0/0"
}

module "open-egress-sg" {
  source = "../open-egress-sg"
  name   = "${var.name_prefix}-ec2-nat-open-egress"
  vpc_id = "${var.vpc_id}"
}

resource "aws_security_group" "ec2-nat" {
  name   = "${var.name_prefix}-ec2-nat"
  vpc_id = "${var.vpc_id}"

  tags {
    Name        = "${var.name_prefix}-ec2-nat"
    Description = "Allow NAT ephemeral ports to hosts in ${var.name_prefix}"
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = "${var.private_subnet_cidrs}"
  }

  egress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
<<<<<<< HEAD
    from_port   = 433
    to_port     = 433
=======
    from_port   = 443
    to_port     = 443
>>>>>>> PT-324-log-aggregation
    protocol    = "tcp"
    cidr_blocks = "${var.private_subnet_cidrs}"
  }

<<<<<<< HEAD
  ingress {
    from_port   = 433
    to_port     = 433
=======
  egress {
    from_port   = 443
    to_port     = 443
>>>>>>> PT-324-log-aggregation
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

data "template_file" "user_data" {
  vars {
    private_subnets = "${join(" ", var.private_subnet_cidrs)}"
  }

  template = <<END_OF_CONFIG
#!/bin/bash
echo '#!/bin/sh
echo 1 > /proc/sys/net/ipv4/ip_forward
for s in $${private_subnets};
do
  logger -i -t "user_data" "Setting NAT for $s subnet"
  iptables -t nat -A POSTROUTING -s $s -j MASQUERADE
done
' > /etc/network/if-pre-up.d/nat-setup
chmod +x /etc/network/if-pre-up.d/nat-setup
/etc/network/if-pre-up.d/nat-setup
END_OF_CONFIG
}

resource "aws_instance" "ec2-nat" {
  source_dest_check           = false                      # Will not work otherwise
  associate_public_ip_address = true
  ami                         = "${module.ubuntu-ami.id}"
  key_name                    = "${var.aws_key_pair}"
  instance_type               = "t2.nano"
  availability_zone           = "${var.availability_zone}"

  root_block_device {
    volume_type = "gp2"
    volume_size = "10"
  }

  vpc_security_group_ids = [
    "${module.public-ssh-sg.id}",
    "${module.open-egress-sg.id}",
    "${aws_security_group.ec2-nat.id}",
  ]

  subnet_id = "${var.public_subnet_ids[0]}"

  tags {
    Name = "${var.name_prefix}-ec2-nat-${count.index}"
  }

  user_data = "${data.template_file.user_data.rendered}"
}

resource "aws_route_table" "ec2-nat" {
  vpc_id = "${var.vpc_id}"

  route {
    cidr_block  = "0.0.0.0/0"
    instance_id = "${aws_instance.ec2-nat.id}"
  }

  tags = "${merge(map("Name", "${var.name_prefix}-ec2-nat-${count.index}"), "${var.extra_tags}")}"
}

resource "aws_route_table_association" "ec2-nat" {
  count          = "${length(var.private_subnet_ids)}"
  subnet_id      = "${var.private_subnet_ids[count.index]}"
  route_table_id = "${aws_route_table.ec2-nat.id}"
}
