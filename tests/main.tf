# THE test VPC with an internet gateway
resource "aws_vpc" "test" {
    cidr_block = "${var.vpc_cidr_prefix}.0.0/16"
    enable_dns_hostnames = true
    tags {
        Name = "test-${var.name}"
    }
}
resource "aws_internet_gateway" "test" {
    vpc_id = "${aws_vpc.test.id}"
    tags {
        Name = "test-${var.name}"
        Region = "${var.region}"
    }
}
resource "aws_route_table" "test" {
    vpc_id = "${aws_vpc.test.id}"
    route {
        cidr_block = "0.0.0.0/0"
        gateway_id = "${aws_internet_gateway.test.id}"
    }
    tags {
        Name = "test-${var.name}"
        Region = "${var.region}"
    }
}
resource "template_file" "leader-init" {
    filename = "scripts/init-leaders.tpl"
    vars {
        region = "${var.region}"
        service = "leaders"
        secret_key = "${var.consul_secret_key}"
        cidr_prefix_a = "${var.cidr_prefix_leader_a}"
        cidr_prefix_c = "${var.cidr_prefix_leader_c}"
        leader_count = 3
        master_token = "${var.consul_master_token}"
        client_token = "${var.consul_master_token}"
    }
}

module "cleaders" {
    source = "../tf-modules/consul-leaders"
    ami = "${var.ami}"
    name = "${var.name}"
    max_nodes = 5
    min_nodes = 3
    desired_capacity = 3
    key_name = "${var.key_name}-leaders"
    key_file = "${var.key_file}"
    ssh_pubkey = "${var.ssh_pubkey}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    consul_secret_key = "${var.consul_secret_key}"
    consul_master_token = "${var.consul_master_token}"
    cidr_prefix_a = "${var.cidr_prefix_leader_a}"
    cidr_prefix_c = "${var.cidr_prefix_leader_c}"
    vpc_id = "${aws_vpc.test.id}"
    route_table_id = "${aws_route_table.test.id}"
    inbound_security_group = "${module.cleader-inbound-sg.id}"
    user_data = "${template_file.leader-init.rendered}"
}

resource "template_file" "worker-init" {
    filename = "scripts/init-worker.tpl"
    vars {
        region = "${var.region}"
        secret_key = "${var.consul_secret_key}"
        master_token = "${var.consul_master_token}"
        leader_dns = "${module.cleaders.leader_dns}"
    }
}

resource "aws_security_group" "worker-service" {
    name = "service-${var.name}-${var.region}"
    vpc_id = "${aws_vpc.test.id}"
    tags {
        Name = "service-${var.name}-${var.region}"
        Description = "Allow TCP and UDP ports for services running on ${var.name} cluster"
    }
    # open ports 3000 to 9999 for testing
    ingress {
        from_port = 3000
        to_port = 9999
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }
}

module "cworkers-a" {
    source = "../tf-modules/consul-cluster"
    ami = "${var.ami}"
    name = "${var.name}"
    max_nodes = 5
    min_nodes = 3
    desired_capacity = 3
    key_name = "${var.key_name}-workers"
    ssh_pubkey = "${var.ssh_pubkey}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    cidr_minions_a = "${var.cidr_minions_a}"
    cidr_minions_c = "${var.cidr_minions_c}"
    vpc_id = "${aws_vpc.test.id}"
    route_table_id = "${aws_route_table.test.id}"
    inbound_security_group = "${module.cworker-inbound-sg.id}"
    service_security_group = "${aws_security_group.worker-service.id}"
    user_data = "${template_file.worker-init.rendered}"
}

# allow worker to leader
module "cleader-inbound-sg" {
    source = "../tf-modules/consul-leader-sg"
    name = "inbound-${var.name}"
    vpc_id = "${aws_vpc.test.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${var.cidr_minions_a},${var.cidr_minions_c}"
}

# and allow leader to worker
module "cworker-inbound-sg" {
    source = "../tf-modules/consul-agent-sg"
    name = "inbound-${var.name}"
    vpc_id = "${aws_vpc.test.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${module.cleaders.subnet-a-cidr_block},${module.cleaders.subnet-c-cidr_block}"
}
