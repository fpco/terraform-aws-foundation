# this SSH key will be used for initial provisioning of each instance in the VPC
resource "aws_key_pair" "main" {
    key_name = "${var.key_name}"
    public_key = "${var.ssh_pubkey}"
}
