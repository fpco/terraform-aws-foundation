/**
 *##Nomad Server Security Group
 *
 * Security group for the Nomad Servers/Leaders.
 */
resource "aws_security_group" "main" {
  vpc_id = "${var.vpc_id}"

  tags {
    Name        = "${var.name}-nomad-server"
    Description = "Allow 4646/4647 (TCP, HTTP/RPC) to both worker/servers, and 4848 (serf, tcp/udp) to servers only in ${var.name}"
  }

  # open port 4646/4647 (nomad-server) tcp for the leaders
  ingress {
    from_port = 4646
    to_port   = 4647
    protocol  = "tcp"

    cidr_blocks = ["${distinct(concat(var.server_cidr_blocks, var.worker_cidr_blocks))}"]
  }

  # open port 4648 (nomad) tcp/udp for the leaders
  ingress {
    from_port   = 4648
    to_port     = 4648
    protocol    = "tcp"
    cidr_blocks = ["${var.server_cidr_blocks}"]
  }

  ingress {
    from_port   = 4648
    to_port     = 4648
    protocol    = "udp"
    cidr_blocks = ["${var.server_cidr_blocks}"]
  }
}
