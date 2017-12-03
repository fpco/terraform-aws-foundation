/**
 * ## EC2 NAT Instance
 *
 * **UPDATE THESE DOCS**
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
  is_govcloud = "${var.aws_cloud == "aws-us-gov" ? "true" : "false"}" # Canonical
}

# boxed module to update the EC2 node's hostname
module "init-nat-hostname" {
  source          = "../init-snippet-hostname-simple"
  hostname_prefix = "ec2-nat"
}

# init snippet to configure NAT
# be super careful making updates to this section, there are several layers
# this code needs to escape through (terraform, init/bash/echo, shell)
module "init-nat-config-iptables" {
  source = "../init-snippet-exec"
  init   = <<END_INIT
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

# create a list of template_file data sources with init for each instance
data "template_file" "nat-init" {
  count    = "${length(var.public_subnet_ids)}"
  template = <<END_INIT
#!/bin/bash
${module.init-nat-hostname.init_snippet}
${module.init-nat-config-iptables.init_snippet}
END_INIT
}

module "ec2-nat" {
  source             = "../ec2-auto-recover-instances"
  # name scheme looks like "name-ec2-nat-01" and so on
  name_prefix        = "${var.name_prefix}"
  name_format        = "%s-ec2-nat-%02d"
  extra_tags         = "${var.extra_tags}"
  aws_cloud          = "${var.aws_cloud}"
  ami                = "${module.ubuntu-ami.id}"
  key_name           = "${var.key_name}"
  instance_type      = "${var.instance_type}"
  iam_profiles       = ["${var.iam_profiles}"]
  security_group_ids = ["${var.security_group_ids}"]
  subnet_ids         = ["${var.public_subnet_ids}"]
  # let AWS choose IPs for these instances
  private_ips        = []
  source_dest_check  = false # NAT requires this
  public_ip          = true  # NAT requires this
  root_volume_size   = "${var.root_volume_size}"
  user_data          = ["${data.template_file.nat-init.*.rendered}"]
}
