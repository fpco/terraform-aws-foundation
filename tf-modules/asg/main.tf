# Auto-Scaling Group
resource "aws_autoscaling_group" "cluster" {
    availability_zones = ["${compact(split(",", replace(var.az_list, " ", "")))}"]
    desired_capacity = "${var.desired_capacity}"
    force_delete = true
    health_check_grace_period = 300
    health_check_type = "EC2"
    launch_configuration = "${aws_launch_configuration.cluster.name}"
    load_balancers =  ["${compact(split(",", replace(var.elb_names, " ", "")))}"]
    max_size = "${var.max_nodes}"
    min_size = "${var.min_nodes}"
    name = "${var.name}-${var.suffix}"
    vpc_zone_identifier = ["${compact(split(",", replace(var.subnet_ids, " ", "")))}"]
}
# Launch Config for the ASG
resource "aws_launch_configuration" "cluster" {
    # omit name so it's generated as a unique value
    associate_public_ip_address = "${var.public_ip}"
    iam_instance_profile = "${var.iam_profile}"
    image_id = "${var.ami}"
    instance_type = "${var.instance_type}"
    key_name = "${var.key_name}"
    security_groups = ["${compact(split(",", replace(var.security_group_ids, " ", "")))}"]
    user_data = "${var.user_data}"
}
