output "net_a" {
  value = "${module.cluster-net.id_a}"
}

output "net_c" {
  value = "${module.cluster-net.id_c}"
}

output "cidr_a" {
  value = "${module.cluster-net.cidr_a}"
}

output "cidr_c" {
  value = "${module.cluster-net.cidr_c}"
}

output "leader_dns" {
  value = "${aws_route53_record.leaders.name}"
}

output "asg_name" {
  value = "${module.leader-asg.name}"
}

output "asg_id" {
  value = "${module.leader-asg.id}"
}
