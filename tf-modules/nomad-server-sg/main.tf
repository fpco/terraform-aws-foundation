# Security group to cover Nomad Server
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.region}-nomad-server"
        Description = "Allow 4648 (TCP/UDP) for the nomad server communication in ${var.name}"
    }
    # open port 4648 (nomad) tcp/udp for the leaders
    ingress {
        from_port = 4648
        to_port = 4648
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
    ingress {
        from_port = 4648
        to_port = 4648
        protocol = "udp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
}
