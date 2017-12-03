/**
 * ## Auto-Recovering EC2 Instances
 *
 * **UPDATE THESE DOCS**
 *
 * Use the `aws_instance` and `aws_cloudwatch_metric_alarm` resources in the
 * following pattern:
 *
 * * run N instances, where N is the number of IP addresses in the `private_ips`
 *   list parameter to the module
 * * set the private IP addresses, don't get something random from AWS
 * * accept arbitrary `user_data`
 * * setup a metric alarm and action to auto-recover instances that fail the
 *   health check
 * * no use of instance store, or dedicated EBS volumes which follow an instance
 *
 * This pattern is useful for:
 *
 * * DNS servers
 * * a server that needs a fixed IP address
 * * a server which should be automatically replaced
 * * a server that does not need some dedicated EBS volume to follow (state is
 *   not important
 *
 * Also note that the number of subnets provided does not need to match the
 * number of private IP addresses. Specifically, the `element()` interpolation
 * function is used for `aws_instance.subnet_id`, and that function will wrap
 * using a standard mod algorithm.
 *
 * ### Example
 *
 *     # The DNS servers
 *     module "dns" {
 *       source = "../../vendor/fpco-terraform-aws/tf-modules/ec2-auto-recover-instances"
 *       name_prefix         = "${var.name}"
 *       ami                 = "${data.aws_ami.ubuntu-xenial.id}"
 *       key_name            = "${aws_key_pair.main.id}"
 *       subnet_ids          = ["${module.private-subnets.ids}"]
 *       private_ips         = ["${var.list_of_ips}"]
 *       security_group_ids  = [
 *         "${module.dns-server-sg.id}",
 *         "${module.public-ssh-sg.id}",
 *         "${module.open-egress-sg.id}",
 *       ]
 *       user_data = <<END_INIT
 *     ufw allow 53
 *     echo "10.10.0.10 foobar.${var.private_dns_zone_name}" >> /etc/hosts
 *     END_INIT
 *       alarm_actions = []
 *     }
 */

# if we have no private_ips in our list, use the subnet_ids to determine how many
# EC2 instances to create. This lets the module create N instances across M subnets
# (interleave), or N instances across N subnets.
data "template_file" "cnt" {
  template = "$${ip_count ? ip_count : subnet_count}"

  vars {
    ip_count     = "${length(var.private_ips)}"
    subnet_count = "${length(var.subnet_ids)}"
  }
}

# The instance running the DNS server
resource "aws_instance" "auto-recover" {
  count                       = "${data.template_file.cnt.rendered}"
  ami                         = "${var.ami}"
  instance_type               = "${var.instance_type}"
  iam_instance_profile        = "${data.template_file.cnt.rendered > 0 ? element(concat(var.iam_profiles, list("")), count.index) : ""}"
  subnet_id                   = "${element(var.subnet_ids, count.index)}"
  vpc_security_group_ids      = ["${var.security_group_ids}"]
  associate_public_ip_address = "${var.public_ip}"
  source_dest_check           = "${var.source_dest_check}"
  private_ip                  = "${data.template_file.cnt.rendered > 0 ? element(concat(var.private_ips, list("")), count.index) : ""}"
  key_name                    = "${var.key_name}"

  root_block_device {
    volume_type = "${var.root_volume_type}"
    volume_size = "${var.root_volume_size}"
  }

  # Instance auto-recovery (see cloudwatch metric alarm below) doesn't support
  # instances with ephemeral storage, so this disables it.
  # See https://github.com/hashicorp/terraform/issues/5388#issuecomment-282480864
  ephemeral_block_device {
    device_name = "/dev/sdb"
    no_device   = true
  }

  ephemeral_block_device {
    device_name = "/dev/sdc"
    no_device   = true
  }

  tags = "${merge(
              map("Name", format(var.name_format, var.name_prefix, count.index + 1)),
	      "${var.extra_tags}"
	 )}"

  lifecycle {
    ignore_changes = ["ami"]
  }

  user_data = "${element(var.user_data, count.index)}"
}

# Current AWS region
data "aws_region" "current" {
  current = true
}

# Cloudwatch alarm that recovers the instance after two minutes of system status
# check failure
resource "aws_cloudwatch_metric_alarm" "auto-recover" {
  count               = "${data.template_file.cnt.rendered}"
  alarm_name          = "${format(var.name_format, var.name_prefix, count.index + 1)}"
  metric_name         = "${var.metric_name}"
  comparison_operator = "${var.comparison_operator}"
  evaluation_periods  = "${var.evaluation_periods}"

  dimensions {
    InstanceId = "${aws_instance.auto-recover.*.id[count.index]}"
  }

  namespace         = "${var.namespace}"
  period            = "${var.max_failure_duration}"
  statistic         = "${var.statistic}"
  threshold         = "${var.threshold}"
  alarm_description = "${var.alarm_description}"
  alarm_actions     = ["${compact(concat(list("arn:${var.aws_cloud}:automate:${data.aws_region.current.name}:ec2:recover"), "${var.alarm_actions}"))}"]
}
