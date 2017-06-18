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
    root_volume_size = "9"
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
df -h
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
    cluster_security_group_ids = "${module.nomad-agent-sg.id}, ${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
    user_data = "${module.worker-init.user_data}"
    root_volume_size = "10"
    root_volume_type = "standard"
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
df -h
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

########################
### Test NAT Gateway ###
########################
# set up public and private subnets
resource "aws_subnet" "public" {
    vpc_id = "${module.test-vpc.id}"
    cidr_block = "10.100.12.0/24"
    map_public_ip_on_launch = true
}
resource "aws_subnet" "private" {
    vpc_id = "${module.test-vpc.id}"
    cidr_block = "10.100.13.0/24"
    map_public_ip_on_launch = false
}
# set up nat gateway
module "nat_gateways" {
    source = "../tf-modules/nat-gateways"
    vpc_id = "${module.test-vpc.id}"
    name_prefix = "${var.name}"
    nat_count = 1
    public_subnet_ids = ["${aws_subnet.public.id}"]
    private_subnet_ids = ["${aws_subnet.private.id}"]
    region = "${var.region}"
}
# route tables for above subnets
resource "aws_route_table" "public" {
    vpc_id = "${module.test-vpc.id}"
    route {
        cidr_block = "0.0.0.0/0"
        gateway_id = "${module.test-vpc.igw_id}"
    }
}
# Route Table Associations
resource "aws_main_route_table_association" "main" {
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${aws_route_table.public.id}"
}
resource "aws_route_table_association" "public" {
    subnet_id = "${aws_subnet.public.id}"
    route_table_id = "${aws_route_table.public.id}"
}
# Security groups
resource "aws_security_group" "public-subnet" {
    name = "$nat-test-public-subnet"
    vpc_id = "${module.test-vpc.id}"
    description = "Allow SSH from Internet, Pass to Private subnet"
    # allow SSH, icmp from anywhere
    ingress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }
    ingress {
        from_port = -1
        to_port = -1
        protocol = "icmp"
        cidr_blocks = ["0.0.0.0/0"]
    }
    # pass to private subnet
    egress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["${aws_subnet.private.cidr_block}"]
    }
    egress {
        from_port = -1
        to_port = -1
        protocol = "icmp"
        cidr_blocks = ["0.0.0.0/0"]
    }
    tags {
        Name = "nat-test-public-subnet"
        Description = "Allow SSH from Internet, Pass to Private subnet"
    }
}
resource "aws_security_group" "private-subnet" {
    name = "nat-test-private-subnet"
    vpc_id = "${module.test-vpc.id}"
    description = "Allow SSH/HTTP/HTTPS from Public Subnet, Outbound HTTP/HTTPS to internet"
    # allow SSH from public subnet
    ingress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["${aws_subnet.public.cidr_block}"]
    }
    # allow icmp from public subnet
    ingress {
        from_port = -1
        to_port = -1
        protocol = "icmp"
        cidr_blocks = ["${aws_subnet.public.cidr_block}"]
    }
    # outbound http/https
    egress {
        from_port = 80
        to_port = 80
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }
    egress {
        from_port = 443
        to_port = 443
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }
    # outbound icmp
    egress {
        from_port = -1
        to_port = -1
        protocol = "icmp"
        cidr_blocks = ["0.0.0.0/0"]
    }
    tags {
        Name = "nat-test-private-subnet"
        Description = "Allow SSH from Public Subnet, Outbound icmp to internet"
    }
}
# instances
resource "aws_instance" "ssh-bastion" {
    ami = "${var.ami}"
    associate_public_ip_address = true
    instance_type = "t2.micro"
    availability_zone = "${aws_subnet.public.availability_zone}"
    depends_on = ["aws_key_pair.tests"]
    key_name = "${var.key_name}"
    security_groups = ["${aws_security_group.public-subnet.id}"]
    subnet_id = "${aws_subnet.public.id}"
    count = 1
    root_block_device = {
        volume_type = "gp2"
        volume_size = "15"
        delete_on_termination = true
    }
    provisioner "file" {
        source = "${var.key_file}"
        destination = "/home/ubuntu/.ssh/${var.key_name}"
        connection {
            user = "ubuntu"
            key_file = "${var.key_file}"
        }
    }
    provisioner "remote-exec" {
        inline = [
        "chown ubuntu:ubuntu /home/ubuntu/.ssh/${var.key_name}",
        "chmod 400 /home/ubuntu/.ssh/${var.key_name}",
        "ls -alh /home/ubuntu/.ssh"
        ]
        connection {
            user = "ubuntu"
            key_file = "${var.key_file}"
        }
    }
    tags {
        Name = "nat-test-ssh-bastion"
    }
}
resource "aws_instance" "private-instance" {
    ami = "${var.ami}"
    associate_public_ip_address = false
    instance_type = "t2.micro"
    availability_zone = "${aws_subnet.private.availability_zone}"
    depends_on = ["aws_key_pair.tests"]
    key_name = "${var.key_name}"
    security_groups = ["${aws_security_group.private-subnet.id}"]
    subnet_id = "${aws_subnet.private.id}"
    count = 1
    root_block_device = {
        volume_type = "gp2"
        volume_size = "15"
        delete_on_termination = true
    }
    tags {
        Name = "nat-test-private-instance"
    }
}
