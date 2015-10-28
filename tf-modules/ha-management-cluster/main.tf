# Security Group, for management-tier service
resource "aws_security_group" "management_services" {
    name = "${var.name}-management-services"
    vpc_id = "${var.vpc_id}"
    # salt-master
#   ingress {
#       from_port = 4505
#       to_port = 4506
#       protocol = "tcp"
#       cidr_blocks = ["${split(",", var.minion_cidr_blocks)}"]
#   }
    # open port 123 (NTP) tcp/udp for the VPC
    ingress {
        from_port = 123
        to_port = 123
        protocol = "tcp"
        cidr_blocks = ["${split(",", var.worker_cidr_blocks)}"]
    }
    ingress {
        from_port = 123
        to_port = 123
        protocol = "udp"
        cidr_blocks = ["${split(",", var.worker_cidr_blocks)}"]
    }
    tags {
        Description = "VPC access to services in the management tier"
    }
}
# provision the management cluster
resource "template_file" "manage_init" {
    filename = "${path.module}/init.tpl"
    vars {
        region = "${var.region}"
        consul_secret_key = "${var.consul_secret_key}"
        consul_master_token = "${var.consul_master_token}"
        leader_dns = "${var.leader_dns}"
    }
}
