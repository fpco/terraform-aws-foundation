# a consul agent cluster is an auto-scaling group..
module "agent-asg" {
    source = "../asg"
    ami = "${var.ami}"
    az_list = "${var.region}a, ${var.region}c"
    desired_capacity = "${var.desired_capacity}"
    elb_names = "${var.load_balancers}"
    max_nodes = "${var.max_nodes}"
    min_nodes = "${var.min_nodes}"
    key_name = "${var.key_name}"
    name = "${var.name}"
    suffix = "cluster"
    cidr_a = "${module.cluster-net.cidr_a}"
    cidr_c = "${module.cluster-net.cidr_c}"
    vpc_id = "${var.vpc_id}"
    route_table_id = "${var.route_table_id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    subnet_ids = "${module.cluster-net.id_a}, ${module.cluster-net.id_c}"
    security_group_ids = "${var.cluster_security_group_ids}"
    user_data = "${var.user_data}"
}
# and a multi-az network
module "cluster-net" {
    source = "../cluster-network"
    name = "${var.name}"
    route_table_id = "${var.route_table_id}"
    vpc_id = "${var.vpc_id}"
    cidr_a = "${var.cidr_minions_a}"
    cidr_c = "${var.cidr_minions_c}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    public_ip = "${var.public_ip}"
}

