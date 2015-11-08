# Security group to cover Consul Agent
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.region}-consul-agent"
        Description = "Allow TCP and UDP ports to consul agent in ${var.name}"
    }
    # Serf LAN, used to handle gossip in the LAN. Required by all agents. TCP and UDP.
    ingress {
        from_port = 8301
        to_port = 8301
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
    ingress {
        from_port = 8301
        to_port = 8301
        protocol = "udp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
}
