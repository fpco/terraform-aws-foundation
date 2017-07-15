module "vpc" {
    source = "../../vpc-network"
    name = "${var.name}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    vpc_cidr_prefix = "10.23"
}
provider "aws" {
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    token = "${var.token}"
}
resource "aws_subnet" "main" {
    availability_zone = "${var.region}${var.az}"
    vpc_id = "${module.vpc.id}"
    cidr_block = "10.23.1.0/24"
    map_public_ip_on_launch = true
    tags {
        Name = "${var.name}-${var.az}"
        Region = "${var.region}"
    }
}
module "public-ssh" {
    source = "../../ssh-sg"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    vpc_id = "${module.vpc.id}"
}
resource "aws_security_group" "open-egress" {
    name = "${var.name}-test-persistent-ebs"
    vpc_id = "${module.vpc.id}"
    # open egress
    egress {
        from_port = 0
        to_port = 0
        protocol = "-1"
        cidr_blocks = ["0.0.0.0/0"]
    }
    tags {
        Description = "open egress for persistent-EBS tests"
    }
}
module "ec2-with-persistent-ebs" {
    source = "../../ec2-attach-persistent-ebs"
    security_group_ids = "${module.public-ssh.id},${aws_security_group.open-egress.id}"
    name = "${var.name}"
    suffix = "test-persistent-ebs"
    instance_type = "t2.micro"
    ami = "ami-7ca35e1c"
    vpc_id = "${module.vpc.id}"
    route_table_id = "${module.vpc.route_table_id}"
    subnet_id = "${aws_subnet.main.id}"
    az = "${var.region}${var.az}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    key_name = "${aws_key_pair.main.key_name}"
    key_file = "${var.key_file}"
}
resource "aws_key_pair" "main" {
    key_name = "${var.key_name}"
    public_key = "${var.ssh_pubkey}"
}
output "key_file" {
    value = "${var.key_file}"
}
output "asg_name" {
    value = "${module.ec2-with-persistent-ebs.asg_name}"
}
# variables for this test environment
variable "name" {
    description = "The name of the environment to deploy to (beta/prod/etc)"
}
variable "key_name" {
    default = ""
    description = "The name of the (AWS) SSH key to associate with the instance"
}
variable "key_file" {
    description = "The path to the SSH private key to provide connection info as output"
}
variable "ssh_pubkey" {
    default = ""
    description = "The public key for SSH"
}
variable "region" {
    description = "The AWS region to deploy to"
    default = ""
}
variable "az" {
    description = "The AWS Availability Zone (AZ) to create the instance in"
}
variable "instance_type" {
    default = "t2.micro"
    description = "The type of AWS instance (size)"
}
variable "access_key" {
    default = ""
}
variable "secret_key" {
    default = ""
}
variable "token" {
    default = ""
}
