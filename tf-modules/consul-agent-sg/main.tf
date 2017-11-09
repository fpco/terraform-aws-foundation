/**
 *## Security Group for Consul Agents
 *
 *This module creates a _single_ `aws_security_group` resource. This resource has
 *an ingress rule to allow port `8301`, for TCP and UDP each. Use the security
 *group on any nodes you wish to use the consul agent on:
 *
 *
 *### Example
 *
 *```
 *# boxed security group for consul leader services, no egress/custom rules
 *module "consul-agent-sg" {
 *    source = "../tf-modules/consul-agent-sg"
 *    name = "${var.name}"
 *    vpc_id = "${module.test-vpc.id}"
 *    access_key = "${var.access_key}"
 *    secret_key = "${var.secret_key}"
 *    cidr_blocks = "${module.test-vpc.cidr_block}"
 *}
 *
 *module "my-cluster" {
 *    source = "../tf-modules/consul-cluster"
 *    ...
 *    cluster_security_group_ids = "${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
 *```
 */
# Security group to cover Consul Agent
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-consul-agent"
        Description = "Allow TCP and UDP ports to consul agent in ${var.name}"
    }
    # Serf LAN, used to handle gossip in the LAN. Required by all agents. TCP and UDP.
    ingress {
        from_port = 8301
        to_port = 8301
        protocol = "tcp"
        cidr_blocks = ["${var.cidr_blocks}"]
    }
    ingress {
        from_port = 8301
        to_port = 8301
        protocol = "udp"
        cidr_blocks = ["${var.cidr_blocks}"]
    }
}
