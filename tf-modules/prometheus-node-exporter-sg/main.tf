/**
 *##Prometheus Node Exporter Security Group
 *
 * Security group for the Prometheus node exporter.
 */
resource "aws_security_group" "main" {
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-prometheus-node-exporter"
        Description = "Allow prometheus server access to node-exporter in ${var.name}"
    }
    # the prometheus node-exporter listens on 9100
    ingress {
        from_port = 9100
        to_port = 9100
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.cidr_blocks, " ", ""))}"]
    }
}
