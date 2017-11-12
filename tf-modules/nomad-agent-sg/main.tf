/**
 *##Nomad Agent Security Group
 *
 * Security group for the Nomad agents.
 *
 * NOTE: need to make the agent port range (20000 to 60000) a variable.
 */
resource "aws_security_group" "main" {
  vpc_id = "${var.vpc_id}"

  tags {
    Name        = "${var.name}-nomad-agent"
    Description = "Allow port 4646 (TCP/HTTP) to nomad agents in ${var.name}"
  }

  # open port 4646 (nomad http) tcp
  ingress {
    from_port   = 4646
    to_port     = 4646
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }

  ingress {
    from_port   = 20000
    to_port     = 60000
    protocol    = "tcp"
    cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
  }
}
