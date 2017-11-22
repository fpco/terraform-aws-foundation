//ID for subnet A
output "subnet_a_id" {
  value = "${module.cluster-net.id_a}"
}

//CIDR block for subnet A
output "subnet_a_cidr" {
  value = "${module.cluster-net.cidr_a}"
}

//ID for subnet C
output "subnet_c_id" {
  value = "${module.cluster-net.id_c}"
}

//CIDR block for subnet C
output "subnet_c_cidr" {
  value = "${module.cluster-net.cidr_c}"
}

//Name of the Auto-Scaling Group
output "asg_name" {
  value = "${module.agent-asg.name}"
}
