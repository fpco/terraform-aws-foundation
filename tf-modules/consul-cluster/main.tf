# COMMENTED OUT UNTIL TERRAFORM CAN BETTER HANDLE MODULE WITHIN MODULE CONFIGURATIONS
# TF BETTER HANDLES MODULE-IN-MODULE, let's retest..
## boxed security group for consul agents, internal communication
#module "cminion-sg" {
#    source = "../agent_sg"
#    name = "internal-${var.name}"
#    vpc_id = "${var.vpc_id}"
#    region = "${var.region}"
#    access_key = "${var.access_key}"
#    secret_key = "${var.secret_key}"
#    cidr_blocks = "${aws_subnet.a.cidr_block},${aws_subnet.c.cidr_block}"
#}


# PUT THE SECURITY GROUP IN HERE INSTEAD OF USE THE MODULE ABOVE
# SEE https://github.com/hashicorp/terraform/issues/1637 FOR MORE INFORMATION
# Security group to cover the Consul Agent's inter-cluster communication
resource "aws_security_group" "minion" {
    name = "${var.name}-${var.region}-consul-agent"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "consul-agent-${var.name}-${var.region}"
        Description = "Allow TCP and UDP ports to consul agent in ${var.name}"
    }
    # Server RPC, required by all agents and leaders. TCP and UDP.
    ingress {
        from_port = 8300
        to_port = 8300
        protocol = "tcp"
        cidr_blocks = ["${aws_subnet.a.cidr_block}", "${aws_subnet.c.cidr_block}"]
    }
    ingress {
        from_port = 8300
        to_port = 8300
        protocol = "udp"
        cidr_blocks = ["${aws_subnet.a.cidr_block}", "${aws_subnet.c.cidr_block}"]
    }
    # Serf LAN, used to handle gossip in the LAN. Required by all agents. TCP and UDP.
    ingress {
        from_port = 8301
        to_port = 8301
        protocol = "tcp"
        cidr_blocks = ["${aws_subnet.a.cidr_block}", "${aws_subnet.c.cidr_block}"]
    }
    ingress {
        from_port = 8301
        to_port = 8301
        protocol = "udp"
        cidr_blocks = ["${aws_subnet.a.cidr_block}", "${aws_subnet.c.cidr_block}"]
    }
    # unrestricted outbound
    egress {
        from_port = 0
        to_port = 0
        protocol = "-1"
        cidr_blocks = ["0.0.0.0/0"]
    }
}

# Auto-Scaling Group
# rename to "agents"
resource "aws_autoscaling_group" "minions" {
    availability_zones = ["${var.region}a", "${var.region}c"]
    depends_on = ["aws_key_pair.cluster-key"]
    desired_capacity = "${var.desired_capacity.minion}"
    force_delete = true
    health_check_grace_period = 300
    health_check_type = "EC2"
    launch_configuration = "${aws_launch_configuration.v1.name}"
    load_balancers = []
    max_size = "${var.max_nodes}"
    min_size = "${var.min_nodes}"
    name = "${var.name}.consul-cluster"
    vpc_zone_identifier = ["${aws_subnet.a.id}", "${aws_subnet.c.id}"]
}
# Launch Config for the consul minions
# rename to "agents"
# let terraform manage the name of the LC
# (or is it the name of the ASG? check ML)
resource "aws_launch_configuration" "v1" {
    associate_public_ip_address = "${var.public_ip}"
    iam_instance_profile = "${var.iam_profile}"
    image_id = "${var.ami}"
    instance_type = "${var.instance_type}"
    key_name = "${var.key_name}"
    name = "${var.name}.consul-cluster"
    security_groups = [
        "${aws_security_group.ssh.id}",
        "${aws_security_group.minion.id}",
        "${var.inbound_security_group}",
        "${var.service_security_group}",
    ]
    user_data = "${var.user_data}"
}

# Subnets in each of two AZ in this region
# (a and c are the only two in all three US regions)
# support 3 AZ / subnets in the future..
# also use the AWS AZ terraform module to lookup the 0,1,2 AZ based on Region
resource "aws_subnet" "a" {
    availability_zone = "${var.region}a"
    cidr_block = "${var.cidr_minions_a}"
    map_public_ip_on_launch = "${var.public_ip}"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.region}-consul-asg-a"
        Desc = "subnet for auto-scaling group of consul minions, for ${var.name}"
    }
}
resource "aws_subnet" "c" {
    availability_zone = "${var.region}c"
    cidr_block = "${var.cidr_minions_c}"
    map_public_ip_on_launch = "${var.public_ip}"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.region}-consul-asg-c"
        Desc = "subnet for auto-scaling group of consul minions, for ${var.name}"
    }
}
# Routing table association for each minion subnet to the VPC IGW
resource "aws_route_table_association" "a" {
    route_table_id = "${var.route_table_id}"
    subnet_id = "${aws_subnet.a.id}"
}
resource "aws_route_table_association" "c" {
    route_table_id = "${var.route_table_id}"
    subnet_id = "${aws_subnet.c.id}"
}

# Security group to allow inbound SSH for administration
resource "aws_security_group" "ssh" {
    name = "${var.name}-${var.region}-consul-minion-ssh"
    vpc_id = "${var.vpc_id}"
    tags {
	# rename to "${var.name}-agent-${var.region}"
        Name = "consul-minion-ssh-${var.name}-${var.region}"
	# reword "..to (consul) agents"
        Description = "Allow SSH to consul minions in ${var.name}"
    }
    # SSH
    ingress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["${var.ssh_cidr_block}"]
    }
}

# this SSH key will land on instances running in the ASG
resource "aws_key_pair" "cluster-key" {
    key_name = "${var.key_name}" 
    public_key = "${var.ssh_pubkey}"
}
