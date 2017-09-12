output "master_node_asg_names" {
  value = ["${aws_autoscaling_group.master-node-asg.*.name}"]
}

output "data_node_asg_names" {
  value = ["${aws_autoscaling_group.data-node-asg.*.name}"]
}

output "master_node_ebs_volume_ids" {
  value = ["${module.master-node-ebs-volumes.volume_ids}"]
}

output "data_node_ebs_volume_ids" {
  value = ["${module.data-node-ebs-volumes.volume_ids}"]
}

output "elasticsearch_dns" {
  value = {
    "dns_name"     = "${var.elasticsearch_dns_name}"
    "elb_dns_name" = "${aws_elb.elasticsearch-elb.dns_name}"
    "elb_zone_id"  = "${aws_elb.elasticsearch-elb.zone_id}"
  }
}
