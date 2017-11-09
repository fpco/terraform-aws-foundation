/**
 *## Consul Leaders Security Group
 *
 *Boxed security group for use with a cluster of consul leaders. These are the
 *ingress rules for leaders connecting to other leaders.
 *
 *See `consul-agents-sg` for Agents, or `consul-leaders-wan-sg` for the
 *cross-WAN leader communication.
 *
 */
resource "aws_security_group" "main" {
  vpc_id = "${var.vpc_id}"

  tags {
    Name        = "${var.name}-consul-leader"
    Description = "Allow TCP and UDP ports to consul leaders in ${var.name}"
  }

  # Server RPC, used by servers to handle incoming requests from other agents. TCP only.
  ingress {
    from_port   = 8300
    to_port     = 8300
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  ingress {
    from_port   = 8300
    to_port     = 8300
    protocol    = "udp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  # Serf LAN, used to handle gossip in the LAN. Required by all agents. TCP and UDP.
  ingress {
    from_port   = 8301
    to_port     = 8301
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  ingress {
    from_port   = 8301
    to_port     = 8301
    protocol    = "udp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  # consul CLI RPC, used by all agents to handle RPC from the CLI. TCP only.
  ingress {
    from_port   = 8400
    to_port     = 8400
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  # HTTP API, used by clients to talk to the HTTP API. TCP only.
  ingress {
    from_port   = 8500
    to_port     = 8500
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  # DNS Interface, used to resolve DNS queries. TCP and UDP.
  ingress {
    from_port   = 8600
    to_port     = 8600
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  ingress {
    from_port   = 8600
    to_port     = 8600
    protocol    = "udp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }
}
