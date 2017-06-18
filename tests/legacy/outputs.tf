output "elasticache_url" {
    value = "${module.my-ec-cluster.redis_url}"
}
output "leader_asg_name" {
    value = "${module.cleaders.asg_name}"
}
output "worker_asg_name" {
    value = "${module.cworkers-a.asg_name}"
}
output "manage_asg_name" {
    value = "${module.management-cluster.asg_name}"
}
output "bastion_dns" {
    value = "${aws_instance.ssh-bastion.public_dns}"
}
output "private_instance_dns" {
    value = "${aws_instance.private-instance.private_dns}"
}
output "key_file" {
    value = "${var.key_file}"
}
output "key_name" {
    value = "${var.key_name}"
}
