# ASG cluster with consul leaders
module "leader-asg" {
    source = "../asg"
    ami = "${var.ami}"
    az_list = "${var.region}a, ${var.region}c"
    elb_names = "${var.load_balancers}"
    instance_type = "${var.instance_type}"
    desired_capacity = "${var.desired_capacity}"
    max_nodes = "${var.max_nodes}"
    min_nodes = "${var.min_nodes}"
    key_name = "${var.key_name}"
    name = "${var.name}"
    suffix = "consul-leaders"
    subnet_ids = "${module.cluster-net.id_a}, ${module.cluster-net.id_c}"
    security_group_ids = "${var.leader_security_group_ids}"
    user_data = "${var.user_data}"
    root_volume_type = "${var.root_volume_type}"
    root_volume_size = "${var.root_volume_size}"
    public_ip = "${var.public_ip}"
}
#
module "cluster-net" {
    source = "../cluster-network"
    name = "${var.name}"
    route_table_id = "${var.route_table_id}"
    vpc_id = "${var.vpc_id}"
    cidr_a = "${var.cidr_prefix_a}.0/${var.cidr_mask}"
    cidr_c = "${var.cidr_prefix_c}.0/${var.cidr_mask}"
    region = "${var.region}"
    public_ip = "${var.public_ip}"
}
