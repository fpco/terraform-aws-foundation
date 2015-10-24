# boxed security group for consul leaders, internal communication
module "cleader-sg" {
    source = "../consul-leader-sg"
    name = "internal-${var.name}"
    vpc_id = "${var.vpc_id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${aws_subnet.a.cidr_block},${aws_subnet.c.cidr_block}"
}

resource "aws_security_group" "open-egress" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-consul-leader-egress-${var.region}"
        Description = "SG for egress rules applied to consul leaders in ${var.name}"
    }
    egress = {
        from_port = 0
        to_port = 0
        protocol = "-1"
        cidr_blocks = ["0.0.0.0/0"]
    }
}

# Auto-Scaling Group
resource "aws_autoscaling_group" "leaders" {
    availability_zones = ["${var.region}a", "${var.region}c"]
    depends_on = ["aws_key_pair.cluster-key"]
    desired_capacity = "${var.desired_capacity}"
    force_delete = true
    health_check_grace_period = 300
    health_check_type = "EC2"
    launch_configuration = "${aws_launch_configuration.leaders.name}"
    max_size = "${var.max_nodes}"
    min_size = "${var.min_nodes}"
    name = "leaders.consul.${var.name}"
    vpc_zone_identifier = ["${aws_subnet.a.id}", "${aws_subnet.c.id}"]
}
# Launch Config for the consul leaders
resource "aws_launch_configuration" "leaders" {
    associate_public_ip_address = "${var.public_ip}"
    image_id = "${var.ami}"
    instance_type = "${var.instance_type}"
    key_name = "${var.key_name}"
    name = "consul-leaders-${var.name}"
    security_groups = [
        "${module.cleader-sg.id}",
        "${var.inbound_security_group}",
        "${aws_security_group.ssh.id}",
        "${aws_security_group.open-egress.id}",
    ]
    user_data = "${var.user_data}"
}

# Subnets in each of two AZ in this region (a and c are the only two in all three US regions)
resource "aws_subnet" "a" {
    availability_zone = "${var.region}a"
    # ex: this takes "10.57.3" and "28" and comes up with "10.57.3.0/28"
    cidr_block = "${var.cidr_prefix_a}.0/${var.cidr_mask}"
    map_public_ip_on_launch = "${var.public_ip}"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "consul-leaders-asg-${var.name}-${var.region}a"
        Desc = "subnet for auto-scaling group of consul leaders, for ${var.name}"
    }
}
resource "aws_subnet" "c" {
    availability_zone = "${var.region}c"
    # ex: this takes "10.57.4" and "28" and comes up with "10.57.4.0/28"
    cidr_block = "${var.cidr_prefix_c}.0/${var.cidr_mask}"
    map_public_ip_on_launch = "${var.public_ip}"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "consul-leaders-asg-${var.name}-${var.region}c"
        Desc = "subnet for auto-scaling group of consul leaders, for ${var.name}"
    }
}
# Routing table association for each leader subnet to the VPC IGW
resource "aws_route_table_association" "a" {
    route_table_id = "${var.route_table_id}"
    subnet_id = "${aws_subnet.a.id}"
}
resource "aws_route_table_association" "c" {
    route_table_id = "${var.route_table_id}"
    subnet_id = "${aws_subnet.c.id}"
}

# Security group, one that has SSH and all ports for Consul
resource "aws_security_group" "ssh" {
    name = "consul-leader-ssh-${var.name}-${var.region}"
    vpc_id = "${var.vpc_id}"
    description = "Allow SSH to consul leaders in ${var.name}"
    tags {
        Name = "consul-leader-ssh-${var.name}-${var.region}"
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

