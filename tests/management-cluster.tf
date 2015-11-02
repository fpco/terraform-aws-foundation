# create management cluster, the cluster to manage the others
module "ha-management-cluster" {
    source = "../tf-modules/consul-cluster"
    public_ip = "${var.public_ip}"
    ami = "${var.ami}"
    name = "${var.name}-management-tier"
    instance_type = "t2.micro"
    max_nodes = "5"
    min_nodes = "2"
    desired_capacity = "3"
    key_name = "${var.name}-management-tier"
    ssh_pubkey = "${var.ssh_pubkey}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    cidr_minions_a = "${var.cidr_manage_a}"
    cidr_minions_c = "${var.cidr_manage_c}"
    route_table_id = "${aws_route_table.test.id}"
    vpc_id = "${aws_vpc.test.id}"
    inbound_security_group = "${module.consul-agent-sg.id}"
    service_security_group = "${module.ha-management.services_security_group}"
    user_data = "${module.ha-management.user_data}"
}
# to provision the management cluster, and open access to its services
module "ha-management" {
    source = "../tf-modules/ha-management-cluster"
    name = "${var.name}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    worker_cidr_blocks = "${aws_vpc.test.cidr_block}"
    vpc_id = "${aws_vpc.test.id}"
    consul_secret_key = "${var.consul_secret_key}"
    consul_master_token = "${var.consul_master_token}"
    leader_dns = "${module.cleaders.leader_dns}"
}
# boxed security group for consul leaders, internal communication
module "consul-agent-sg" {
    source = "../tf-modules/consul-agent-sg"
    name = "${var.name}-consul-agent"
    vpc_id = "${aws_vpc.test.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${aws_vpc.test.cidr_block}"
}
