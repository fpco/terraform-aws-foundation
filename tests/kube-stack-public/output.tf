// IP address of the bastion jump box
output "bastion_ip" {
    value = "${aws_instance.bastion.public_ip}"
}
// AWS region of deployment
output "region" {
  value = "${var.region}"
}
// Id of the created VPC
output "vpc_id" {
  value = "${module.vpc.vpc_id}"
}
// Ids of created public subnets
output "public_subnet_ids" {
  value = ["${module.vpc.public_subnet_ids}"]
}
// SSH Key pair name
output "ssh_key_name" {
  value = "${aws_key_pair.main.key_name}"
}
