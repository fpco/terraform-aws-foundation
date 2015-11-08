# Security group to cover Nomad Agents
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.region}-nomad-server"
        Description = "Allow ports 4646/4647 (TCP) to nomad agents in ${var.name}"
    }
    # open port 4646/7 (nomad) tcp
    ingress {
        from_port = 4646
        to_port = 4647
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
}
