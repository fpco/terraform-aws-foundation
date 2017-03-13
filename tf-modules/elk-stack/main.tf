/**
 *## ELK stack (ELasticsearch + Logstash + Kibana)
 *
 * This module takes care of deployment of the full ELK stack. Which entails:
 *
 * * Creation of VPC with private and private subnets across many
 *   Availability Zones (AZs). At most one NAT gateway per private subnet,
 *   actual number of gateways is calculated automatically.
 * * Deploying Elasticsearch cluster across a private subnets with specified number
 *   of master and data nodes across all AZs, thus promoting high availability.
 *   See `../elasticsearch` module for more information.
 * * Deploys multiple load balanced servers running Kibana+Logstash each. See
 *   `../logstash+kibana` module for more information, as well as individual modules
 *   `../logstash` and `../kibana`.
 * * It also deploys a control EC2 instance that can be used to manage all instances
 *    in the stack
 */

provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  token = "${var.token}"
  region = "${var.region}"
}


data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["099720109477"] # Canonical
  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-*"]
  }
  filter {
    name   = "root-device-type"
    values = ["ebs"]
  }
}

# module "vpc" {
#   source = "../vpc"

#   azs                  = ["${var.vpc_azs}"]
#   cidr                 = "${var.vpc_cidr}"
#   name_prefix          = "${var.name_prefix}"
#   public_subnet_cidrs  = ["${var.vpc_public_subnet_cidrs}"]
#   private_subnet_cidrs = ["${var.vpc_private_subnet_cidrs}"]
#   region               = "${var.region}"
#   extra_tags           = {}
#   nat_count            = "${length(var.vpc_azs) % floor(max(var.elasticsearch_data_node_count, var.elasticsearch_master_node_count) + 1)}"
# }
# resource "aws_route53_zone_association" "e1c-net" {
#   zone_id = "${var.route53_zone_id}"
#   vpc_id  = "${module.vpc.vpc_id}"
# }

module "subnets" {
  source               = "../subnets"
  azs                  = "${var.vpc_azs}"
  vpc_id               = "${var.vpc_id}"
  name_prefix          = "${var.name_prefix}"
  public_subnet_cidrs  = "${var.vpc_public_subnet_cidrs}"
  private_subnet_cidrs = "${var.vpc_private_subnet_cidrs}"
}

# Give internet to public subnets.
resource "aws_route_table_association" "public-rta" {
  count          = "${length(var.vpc_public_subnet_cidrs)}"
  subnet_id      = "${element(module.subnets.public_ids, count.index)}"
  route_table_id = "${var.vpc_route_table_id}"
}

module "nat-gateways" {
  source             = "../nat-gateways"
  vpc_id             = "${var.vpc_id}"
  name_prefix        = "${var.name_prefix}"
  nat_count          = "${length(var.vpc_azs) % floor(max(var.elasticsearch_data_node_count, var.elasticsearch_master_node_count) + 1)}"
  public_subnet_ids  = ["${module.subnets.public_ids}"]
  private_subnet_ids = ["${module.subnets.private_ids}"]
}


module "elasticsearch" {
  source = "../elasticsearch"

  name_prefix               = "${var.name_prefix}"
  region                    = "${var.region}"
  vpc_id                    = "${var.vpc_id}"
  vpc_azs                   = ["${var.vpc_azs}"]
  route53_zone_id           = "${var.route53_zone_id}"
  key_name                  = "${aws_key_pair.elk-key.key_name}"
  vpc_public_subnet_cidrs   = ["${var.vpc_public_subnet_cidrs}"]
  vpc_private_subnet_cidrs  = ["${var.vpc_private_subnet_cidrs}"]
  vpc_private_subnet_ids    = ["${module.subnets.private_ids}"]
  node_ami                  = "${data.aws_ami.ubuntu.id}"
  data_node_count           = "${var.elasticsearch_data_node_count}"
  data_node_ebs_size        = "${var.elasticsearch_data_node_ebs_size}"
  data_node_snapshot_ids    = ["${var.elasticsearch_data_node_snapshot_ids}"]
  data_node_instance_type   = "${var.elasticsearch_data_node_instance_type}"
  master_node_count         = "${var.elasticsearch_master_node_count}"
  master_node_ebs_size      = "${var.elasticsearch_master_node_ebs_size}"
  master_node_snapshot_ids  = ["${var.elasticsearch_master_node_snapshot_ids}"]
  master_node_instance_type = "${var.elasticsearch_master_node_instance_type}"
}


module "kibana" {
  source = "../kibana"

  name_prefix          = "${var.name_prefix}"
  vpc_id               = "${var.vpc_id}"
  vpc_azs              = ["${var.vpc_azs}"]
  route53_zone_id      = "${var.route53_zone_id}"
  kibana_dns_name      = "${var.kibana_dns_name}"
  subnet_ids           = ["${module.subnets.public_ids}"]
  key_name             = ""
  ami                  = ""
  instance_type        = ""
  elasticsearch_url    = "http://${module.elasticsearch.elb_dns}:9200"
  min_server_count     = 0
  max_server_count     = 0
  desired_server_count = 0
}

module "logstash-kibana" {
  source = "../logstash"

  name_prefix              = "${var.name_prefix}-kibana"
  vpc_id                   = "${var.vpc_id}"
  subnet_ids               = ["${module.subnets.public_ids}"]
  vpc_azs                  = ["${var.vpc_azs}"]
  route53_zone_id          = "${var.route53_zone_id}"
  logstash_dns_name        = "${var.logstash_dns_name}"
  ami                      = "${data.aws_ami.ubuntu.id}"
  instance_type            = "${var.logstash_kibana_instance_type}"
  key_name                 = "${aws_key_pair.elk-key.key_name}"
  elasticsearch_url        = "http://${module.elasticsearch.elb_dns}:9200"
  min_server_count         = "${var.logstash_kibana_min_server_count}"
  max_server_count         = "${var.logstash_kibana_max_server_count}"
  desired_server_count     = "${var.logstash_kibana_desired_server_count}"
  extra_security_groups    = ["${module.kibana.security_group_id}"]
  extra_setup_snippet      = "${module.kibana.setup_snippet}"
  extra_elbs               = ["${module.kibana.elb_name}"]
  certstrap_depot_path     = "${var.certstrap_depot_path}"
  certstrap_ca_common_name = "${var.certstrap_ca_common_name}"
  certstrap_ca_passphrase  = "${var.certstrap_ca_passphrase}"
  credstash_table_name     = "${var.credstash_table_name}"
  credstash_kms_key_arn    = "${var.credstash_kms_key_arn}"
  credstash_prefix         = "${var.name_prefix}-"
}


resource "aws_key_pair" "elk-key" {
  key_name = "${var.name_prefix}-key"
  public_key = "${file("${path.module}/${var.pub_key_file}")}"
}


resource "aws_instance" "control-instance" {
  count                       = "${var.deploy_control_instance}"
  ami                         = "${data.aws_ami.ubuntu.id}"
  instance_type               = "t2.nano"
  subnet_id                   = "${module.subnets.public_ids[0]}"
  vpc_security_group_ids      = ["${aws_security_group.control-instance-sg.id}"]
  key_name                    = "${aws_key_pair.elk-key.key_name}"
  associate_public_ip_address = true

  tags {
    Name = "${var.name_prefix}-control-instance"
  }

}

resource "aws_security_group" "control-instance-sg" {
  count       = "${var.deploy_control_instance}"
  name        = "${var.name_prefix}-control-instance-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow SSH, ICMP, Elasticsearch TCP, Elasticsearch HTTP, and everything outbound."

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

}

//Control instance public IP address. If list is empty, control instance wasn't deployed.
output "control_instance_public_ip" {
  value = ["${aws_instance.control-instance.*.public_ip}"]
}

//Elasticsearch Internal Load Balancer DNS.
output "elasticsearch_internal_elb_dns" {
  value = "${module.elasticsearch.elb_dns}"
}

# //Logstash Load Balancer DNS.
# output "logstash_elb_dns" {
#   value = "${module.logstash-kibana.logstash_elb_dns}"
# }

# //Kibana Load Balancer DNS.
# output "kibana_elb_dns" {
#   value = "${module.logstash-kibana.kibana_elb_dns}"
# }


//Running this command will setup SOCKS proxy to VPC through control instance.
output "socket_cmd" {
  value = "ssh -i ${var.priv_key_file} -D 8123 -f -C -q -N -o StrictHostKeyChecking=no -o LogLevel=ERROR -o UserKnownHostsFile=/dev/null ubuntu@${aws_instance.control-instance.public_ip}"
}
