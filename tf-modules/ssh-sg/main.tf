# Security group to allow inbound SSH for administration
resource "aws_security_group" "ssh" {
    name = "${var.name}-ssh"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-ssh"
	# reword "..to (consul) agents"
        Description = "Allow SSH to hosts in ${var.name}"
    }
    # SSH
    ingress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.allowed_cidr_blocks, " ", ""))}"]
    }
}
