# to provision the management cluster, and open access to its services
module "management-cluster" {
    source = "../tf-modules/ha-management-cluster"
    name = "${var.name}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    worker_cidr_blocks = "${module.test-vpc.cidr_block}"
    ami = "${var.ami}"
    key_name = "${aws_key_pair.tests.key_name}"
    cidr_a = "${var.cidr_manage_a}"
    cidr_c = "${var.cidr_manage_c}"
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${module.test-vpc.route_table_id}"
    consul_secret_key = "${var.consul_secret_key}"
    consul_client_token = "${var.consul_master_token}"
    consul_leader_dns = "${module.cleaders.leader_dns}"
    security_group_ids = "${module.nomad-agent-sg.id}, ${module.consul-agent-sg.id}, ${module.public-ssh-sg.id}"
    root_volume_size = "10"
    root_volume_type = "standard"
}

# boxed security group for nomad leader services, no egress/custom rules
module "nomad-server-sg" {
    source = "../tf-modules/nomad-server-sg"
    name = "${var.name}-nomad-server-services"
    vpc_id = "${module.test-vpc.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    server_cidr_blocks = "${var.cidr_prefix_leader_a}.0/28, ${var.cidr_prefix_leader_c}.0/28"
    worker_cidr_blocks = "${var.cidr_manage_a}, ${var.cidr_manage_c}"
}
# boxed security group for nomad agents (leaders included), no egress/custom rules
module "nomad-agent-sg" {
    source = "../tf-modules/nomad-agent-sg"
    name = "${var.name}-nomad-agent"
    vpc_id = "${module.test-vpc.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${module.test-vpc.cidr_block}"
}
