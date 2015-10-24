output "subnet-a-cidr_block" {
    value = "${aws_subnet.a.cidr_block}"
}
output "subnet-c-cidr_block" {
    value = "${aws_subnet.c.cidr_block}"
}
output "leader_dns" {
    value = "${aws_route53_record.leaders.name}"
}
output "asg_name" {
    value = "${aws_autoscaling_group.leaders.name}"
}
output "asg_id" {
    value = "${aws_autoscaling_group.leaders.id}"
}
output "lc_name" {
    value = "${aws_launch_configuration.leaders.name}"
}
