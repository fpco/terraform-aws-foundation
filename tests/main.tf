module "test-vpc" {
    source = "../tf-modules/vpc-network"
    name = "${var.name}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    vpc_cidr_prefix = "${var.vpc_cidr_prefix}"
}
# cluster of consul leaders
module "cleaders" {
    source = "../tf-modules/consul-leaders"
    ami = "${var.ami}"
    name = "${var.name}"
    max_nodes = 5
    min_nodes = 3
    desired_capacity = 3
    key_name = "${aws_key_pair.tests.key_name}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    consul_secret_key = "${var.consul_secret_key}"
    consul_master_token = "${var.consul_master_token}"
    cidr_prefix_a = "${var.cidr_prefix_leader_a}"
    cidr_prefix_c = "${var.cidr_prefix_leader_c}"
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${module.test-vpc.route_table_id}"
    leader_security_group_ids = "${module.consul-agent-sg.id}, ${module.consul-leader-sg.id}, ${aws_security_group.leader-service.id}, ${module.public-ssh-sg.id}"
    user_data = "${module.leader-init.user_data}"
}
# provision consul leaders
module "leader-init" {
    source = "../tf-modules/consul-leaders-generic-init"
    datacenter = "${var.region}"
    cidr_prefix_a = "${var.cidr_prefix_leader_a}"
    cidr_prefix_c = "${var.cidr_prefix_leader_c}"
    consul_secret_key = "${var.consul_secret_key}"
    consul_client_token = "${var.consul_master_token}"
    consul_master_token = "${var.consul_master_token}"
    # use extra_* to customize init..
    extra_pillar = "extra: pillar"
    extra_init = <<EOF
echo "customize this node's init.."
date
consul --version
salt-call --version
uname -a
EOF
    #hostname_prefix = "custom"
}
# boxed security group for consul leader services, no egress/custom rules
module "consul-leader-sg" {
    source = "../tf-modules/consul-leader-sg"
    name = "${var.name}-consul-leader-services"
    vpc_id = "${module.test-vpc.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${module.test-vpc.cidr_block}"
}
# define security group for the custom services and egress on the leader
resource "aws_security_group" "leader-service" {
    name = "${var.name}-leader-custom-${var.region}"
    vpc_id = "${module.test-vpc.id}"
    tags {
        Name = "${var.name}-leader-service-${var.region}"
        Description = "Allow TCP and UDP ports for services running on ${var.name} cluster"
    }
    # open ports 3000 to 9999 for testing
    #ingress {
    #    from_port = 3000
    #    to_port = 9999
    #    protocol = "tcp"
    #    cidr_blocks = ["0.0.0.0/0"]
    #}
    # unrestricted outbound
    egress {
        from_port = 0
        to_port = 0
        protocol = "-1"
        cidr_blocks = ["0.0.0.0/0"]
    }
}

resource "aws_security_group" "worker-service" {
    name = "service-${var.name}-${var.region}"
    vpc_id = "${module.test-vpc.id}"
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
    # unrestricted outbound
    egress {
        from_port = 0
        to_port = 0
        protocol = "-1"
        cidr_blocks = ["0.0.0.0/0"]
    }
}
# cluster of workers, built on cluster of consul agents
module "cworkers-a" {
    source = "../tf-modules/consul-cluster"
    ami = "${var.ami}"
    name = "${var.name}"
    max_nodes = 5
    min_nodes = 3
    desired_capacity = 3
    key_name = "${aws_key_pair.tests.key_name}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    cidr_minions_a = "${var.cidr_minions_a}"
    cidr_minions_c = "${var.cidr_minions_c}"
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${module.test-vpc.route_table_id}"
    cluster_security_group_ids = "${module.management-cluster.nomad_agent_sg}, ${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
    user_data = "${module.worker-init.user_data}"
}
# provisioning for worker cluster
module "worker-init" {
    source = "../tf-modules/consul-agent-generic-init"
    region = "${var.region}"
    service = "worker"
    consul_secret_key = "${var.consul_secret_key}"
    consul_client_token = "${var.consul_master_token}"
    leader_dns = "${module.cleaders.leader_dns}"
    extra_pillar = "extra: pillar"
    extra_init = <<EOF
echo "customize this node's init.."
date
consul --version
salt-call --version
uname -a
EOF
}
# boxed security group for consul agent service - allow whole VPC
module "consul-agent-sg" {
    source = "../tf-modules/consul-agent-sg"
    name = "${var.name}"
    vpc_id = "${module.test-vpc.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${module.test-vpc.cidr_block}"
}
# shared security group for SSH
module "public-ssh-sg" {
    source = "../tf-modules/ssh-sg"
    name = "${var.name}-consul-cluster"
    allowed_cidr_blocks = "0.0.0.0/0"
    vpc_id = "${module.test-vpc.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
}
# this SSH key will land on all instances running in the VPC
# but we could easily create multiple keys, deploying select
# keys to specific clusters
resource "aws_key_pair" "tests" {
    key_name = "${var.key_name}" 
    public_key = "${var.ssh_pubkey}"
}
