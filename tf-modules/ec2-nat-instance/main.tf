/*
 * ## EC2 NAT Instance
 *
 * This module creates a single EC2 instance in one public subnet, to provide
 * NAT to one or more private subnets. The instance will use the latest Ubuntu
 * 16.04 AMI.
 *
 * The module will output the instance ID for use in configuring a route table
 * for those private subnets.
 *
 */

module "ubuntu-ami" {
  source      = "../ami-ubuntu"
  release     = "16.04"
  is_govcloud = "${var.is_govcloud}"
}

# boxed module to update the EC2 node's hostname
module "init-nat-hostname" {
  source          = "../init-snippet-hostname-simple"
  hostname_prefix = "ec2-nat"
}

# init snippet to configure NAT
module "init-nat-config-iptables" {
  source = "../init-snippet-exec"

  init = <<END_INIT
# write out script to setup nat
echo '#!/bin/sh
echo 1 > /proc/sys/net/ipv4/ip_forward
for s in ${join(" ", var.private_subnet_cidrs)};
do
  logger -i -t "user_data" "Setting NAT for $$s subnet"
  iptables -t nat -A POSTROUTING -s $$s -j MASQUERADE
done
' > /etc/network/if-pre-up.d/nat-setup
# make the script executable
chmod +x /etc/network/if-pre-up.d/nat-setup
# run the script
/etc/network/if-pre-up.d/nat-setup
END_INIT
}

resource "aws_instance" "ec2-nat" {
  source_dest_check           = false                         # NAT requires this
  associate_public_ip_address = true                          # NAT requires this
  ami                         = "${module.ubuntu-ami.id}"
  key_name                    = "${var.key_name}"
  instance_type               = "${var.instance_type}"
  availability_zone           = "${data.aws_subnet.public-subnet.availability_zone}"
  vpc_security_group_ids      = ["${var.security_group_ids}"]
  subnet_id                   = "${var.public_subnet_id}"

  root_block_device {
    volume_type = "gp2"
    volume_size = "10"
  }

  lifecycle {
    ignore_changes = ["user_data"]
  }

  tags = "${merge(map("Name", "${var.name_prefix}-ec2-nat"), "${var.extra_tags}")}"

  user_data = <<END_INIT
#!/bin/bash
${module.init-nat-hostname.init_snippet}
${module.init-nat-config-iptables.init_snippet}
END_INIT
}

data "aws_subnet" "public-subnet" {
  id = "${var.public_subnet_id}"
}
