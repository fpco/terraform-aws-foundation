/**
 *## ASG (Autoscaling Group)
 *
 *The purpose of this module is to provide a Launch Configuration and Autoscaling
 *Group as a pair.
 *
 *The ASG supports spanning two Availability Zones and load balancers may be
 *specified. At present, the health checks are defaults and hardcoded (easy to
 *change). The Launch Configuration supports an arbitrary list of security groups,
 *public IPs may be enabled/disabled, and all important details (instance type,
 *ami, key, user data, iam profile) are specified as variables in the modules.
 */
# Auto-Scaling Group
resource "aws_autoscaling_group" "cluster" {
    desired_capacity = "${var.desired_capacity}"
    force_delete = true
    health_check_grace_period = 300
    health_check_type = "EC2"
    launch_configuration = "${aws_launch_configuration.cluster.name}"
    load_balancers =  ["${var.elb_names}"]
    max_size = "${var.max_nodes}"
    min_size = "${var.min_nodes}"
    name = "${var.name}-${var.suffix}"
    lifecycle {
      create_before_destroy = true
    }
    vpc_zone_identifier = ["${var.subnet_ids}"]
  availability_zones        = ["${var.azs}"]
}
# Launch Config for the ASG
resource "aws_launch_configuration" "cluster" {
    # omit name so it's generated as a unique value
    associate_public_ip_address = "${var.public_ip}"
    iam_instance_profile = "${var.iam_profile}"
    image_id = "${var.ami}"
    instance_type = "${var.instance_type}"
    key_name = "${var.key_name}"
    security_groups = ["${var.security_group_ids}"]
    user_data = "${var.user_data}"
    root_block_device {
        volume_type = "${var.root_volume_type}"
        volume_size = "${var.root_volume_size}"
    }
    lifecycle {
      create_before_destroy = true
    }
}
