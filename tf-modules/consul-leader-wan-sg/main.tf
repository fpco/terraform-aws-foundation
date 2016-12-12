/**
 *## Consul Leaders WAN Security Group
 *
 *Boxed security group for use with a cluster of consul leaders. These are the
 *ingress rules for leaders connecting to other leaders across the WAN.
 *
 *See `consul-agents-sg` for Agents, or `consul-leaders-sg` for the
 *inter-datacenter leader communication.
 *
 */
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-consul-leader-wan"
        Description = "Allow TCP and UDP to WAN port (${var.wan_port}) for consul leaders in ${var.name}"
    }
    # Serf WAN, used by servers to gossip over the WAN to other servers. TCP and UDP.
    ingress {
        from_port = ${var.wan_port}
        to_port = ${var.wan_port}
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
    ingress {
        from_port = ${var.wan_port}
        to_port = ${var.wan_port}
        protocol = "udp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
}
