/**
 * ## ASG (Autoscaling Group)
 *
 * The purpose of this module is to provide a Launch Configuration and Autoscaling
 * Group as a pair.
 *
 * The module supports:
 *
 * * spanning N Availability Zones
 * * load balancers may be associated with the ASG
 * * the health checks are not yet parametized, (easy to change)
 * * the Launch Configuration supports an arbitrary list of security groups
 * * `lifecycle` and `create_before_destroy` are used to ensure updates are graceful
 * * public IPs may be enabled/disabled
 * * supports appending `extra_tags`
 * * all important details (instance type, ami, key, user data, iam profile) are
 *   specified as variables in the modules.
 *
 * Note that, Terraform does not run a rolling update when an ASG/LC pair have
 * changed. After the ASG/LC have been updated, the EC2 instances running before
 * the update will still be running. As a result, you will need to terminate each
 * of those instances to perform a rolling update.
 *
 */
# Auto-Scaling Group
resource "aws_autoscaling_group" "cluster" {
  availability_zones        = ["${var.azs}"]
  desired_capacity          = "${var.desired_capacity}"
  force_delete              = true
  health_check_grace_period = 300
  health_check_type         = "EC2"
  launch_configuration      = "${aws_launch_configuration.cluster.name}"
  load_balancers            = ["${var.elb_names}"]
  max_size                  = "${var.max_nodes}"
  min_size                  = "${var.min_nodes}"
  name                      = "${var.name_prefix}-${var.name_suffix}"
  placement_group           = "${var.placement_group}"

  lifecycle {
    create_before_destroy = true
  }

  vpc_zone_identifier = ["${var.subnet_ids}"]

  tags = ["${concat(
    list(
      map("key", "Name",
          "value", "${var.name_prefix}-${var.name_suffix}",
          "propagate_at_launch", true)
      ),
    var.extra_tags
  )}"]
}

# Launch Config for the ASG
resource "aws_launch_configuration" "cluster" {
  # omit name so it's generated as a unique value
  associate_public_ip_address = "${var.public_ip}"
  iam_instance_profile        = "${var.iam_profile}"
  image_id                    = "${var.ami}"
  instance_type               = "${var.instance_type}"
  key_name                    = "${var.key_name}"
  security_groups             = ["${var.security_group_ids}"]
  user_data                   = "${var.user_data}"

  root_block_device {
    volume_type = "${var.root_volume_type}"
    volume_size = "${var.root_volume_size}"
  }

  lifecycle {
    create_before_destroy = true
  }
}
