# -------------------------------------------------------------------------------
# Resources
# -------------------------------------------------------------------------------
data "aws_region" "current" {}

data "aws_caller_identity" "current" {}

# Cloud init script for the autoscaling group
data "template_file" "main" {
  template = "${file("${path.module}/cloud-config.yml")}"

  vars {
    region          = "${data.aws_region.current.name}"
    stack_name      = "${var.name_prefix}-asg"
    lifecycle_topic = "${aws_sns_topic.main.arn}"
    elb_arn         = "${var.elb_arn}"
  }
}

resource "aws_launch_configuration" "main" {
  name_prefix          = "${var.name_prefix}"
  image_id             = "${var.instance_ami}"
  instance_type        = "${var.instance_type}"
  key_name             = "${var.instance_key}"
  iam_instance_profile = "${aws_iam_instance_profile.ec2.name}"
  security_groups      = ["${aws_security_group.main.id}", "${var.elb_sg_id}"]

  user_data = "${data.template_file.main.rendered}"

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_autoscaling_group" "main" {
  name                 = "${var.name_prefix}-${aws_launch_configuration.main.name}"
  launch_configuration = "${aws_launch_configuration.main.id}"
  availability_zones   = ["${var.azs}"]
  load_balancers       = ["${var.elb_names}"]
  vpc_zone_identifier  = ["${var.subnet_ids}"]

  min_size         = "1"
  desired_capacity = "${var.instance_count}"
  max_size         = "4"

  lifecycle {
    create_before_destroy = true
  }

  initial_lifecycle_hook {
    name                    = "${var.name_prefix}-lifecycle"
    default_result          = "CONTINUE"
    heartbeat_timeout       = 60
    lifecycle_transition    = "autoscaling:EC2_INSTANCE_TERMINATING"
    notification_target_arn = "${aws_sns_topic.main.arn}"
    role_arn                = "${aws_iam_role.lifecycle_hook.arn}"
  }
}

resource "aws_security_group" "main" {
  name        = "${var.name_prefix}-sg"
  description = "Allow access to lifecycled instances"
  vpc_id      = "${var.vpc_id}"

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Allow SSH ingress if a EC2 key pair is specified.
resource "aws_security_group_rule" "ssh_ingress" {
  count             = "${var.instance_key != "" ? 1 : 0}"
  security_group_id = "${aws_security_group.main.id}"
  type              = "ingress"
  protocol          = "tcp"
  from_port         = 22
  to_port           = 22
  cidr_blocks       = ["0.0.0.0/0"]
}

# Instance profile for the autoscaling group.
data "aws_iam_policy_document" "permissions" {
  statement {
    effect = "Allow"

    actions = [
      "logs:DescribeLogStreams",
    ]

    resources = [
      "*",
    ]
  }

  statement {
    effect = "Allow"

    actions = [
      "sns:Subscribe",
      "sns:Unsubscribe",
    ]

    resources = [
      "${aws_sns_topic.main.arn}",
    ]
  }

  statement {
    effect = "Allow"

    actions = [
      "sqs:*",
    ]

    resources = ["arn:aws:sqs:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:lifecycled-*"]
  }

  statement {
    effect = "Allow"

    actions = [
      "autoscaling:RecordLifecycleActionHeartbeat",
      "autoscaling:CompleteLifecycleAction",
    ]

    resources = ["*"]
  }

  statement {
    effect = "Allow"

    actions = [
      "elasticloadbalancing:DeregisterInstancesFromLoadBalancer",
      "ec2:DescribeClassicLinkInstances",
      "ec2:DescribeInstances"
    ]

    resources = ["*"]
  }
}

resource "aws_iam_instance_profile" "ec2" {
  name = "${var.name_prefix}-ec2-instance-profile"
  role = "${aws_iam_role.ec2.name}"
}

resource "aws_iam_role" "ec2" {
  name               = "${var.name_prefix}-ec2-role"
  assume_role_policy = "${data.aws_iam_policy_document.ec2_assume.json}"
}

resource "aws_iam_role_policy" "ec2" {
  name   = "${var.name_prefix}-ec2-permissions"
  role   = "${aws_iam_role.ec2.id}"
  policy = "${data.aws_iam_policy_document.permissions.json}"
}

data "aws_iam_policy_document" "ec2_assume" {
  statement {
    effect  = "Allow"
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
  }
}

# SNS topic for the lifecycle hook
resource "aws_sns_topic" "main" {
  name = "${var.name_prefix}-lifecycle"
}

# Execution role and policies for the lifecycle hook
resource "aws_iam_role" "lifecycle_hook" {
  name               = "${var.name_prefix}-lifecycle-role"
  assume_role_policy = "${data.aws_iam_policy_document.asg_assume.json}"
}

resource "aws_iam_role_policy" "lifecycle_hook" {
  name   = "${var.name_prefix}-lifecycle-asg-permissions"
  role   = "${aws_iam_role.lifecycle_hook.id}"
  policy = "${data.aws_iam_policy_document.asg_permissions.json}"
}

data "aws_iam_policy_document" "asg_assume" {
  statement {
    effect  = "Allow"
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["autoscaling.amazonaws.com"]
    }
  }
}

data "aws_iam_policy_document" "asg_permissions" {
  statement {
    effect = "Allow"

    resources = [
      "${aws_sns_topic.main.arn}",
    ]

    actions = [
      "sns:Publish",
    ]
  }
}
