# Security group to cover Consul Leaders
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.region}-consul-leader-wan"
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
