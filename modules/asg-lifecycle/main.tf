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
  name                 = "${var.name_prefix}-${aws_launch_configuration.cluster.name}"
  launch_configuration = aws_launch_configuration.cluster.id
  availability_zones   = var.azs
  load_balancers       = var.elb_names
  vpc_zone_identifier  = var.subnet_ids

  min_size         = "1"
  desired_capacity = var.instance_count
  max_size         = "4"

  lifecycle {
    create_before_destroy = true
  }

  initial_lifecycle_hook {
    name                    = "${var.name_prefix}-lifecycle"
    default_result          = "CONTINUE"
    heartbeat_timeout       = 60
    lifecycle_transition    = "autoscaling:EC2_INSTANCE_TERMINATING"
    notification_target_arn = var.sns_topic_arn
    role_arn                = var.aws_role_arn
  }
}

# Launch Config for the ASG
resource "aws_launch_configuration" "cluster" {
  name_prefix          = var.name_prefix
  image_id             = var.instance_ami
  instance_type        = var.instance_type
  key_name             = var.instance_key
  iam_instance_profile = var.aws_instance_ec2_name
  security_groups      = [var.aws_sg_id, var.elb_sg_id]

  user_data = var.asg_template_file

  lifecycle {
    create_before_destroy = true
  }
}

