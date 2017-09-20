// AMI that was used for all EC2 instances in the stack
output "ami" {
  value = "${data.aws_ami.ubuntu.id}"
}

// Security group that allows SSH access
output "ssh_sg_id" {
  value = "${aws_security_group.ssh.id}"
}

output "elasticsearch_master_node_asg_names" {
  value = ["${module.elasticsearch.master_node_asg_names}"]
}

output "elasticsearch_data_node_asg_names" {
  value = ["${module.elasticsearch.data_node_asg_names}"]
}

output "elasticsearch_master_node_ebs_volume_ids" {
  value = ["${module.elasticsearch.master_node_ebs_volume_ids}"]
}

output "elasticsearch_data_node_ebs_volume_ids" {
  value = ["${module.elasticsearch.data_node_ebs_volume_ids}"]
}

output "logstash_kibana_asg_name" {
  value = "${module.logstash-kibana.asg_name}"
}

output "dns" {
  value = {
    "elasticsearch" = "${module.elasticsearch.elasticsearch_dns}"
    "logstash"      = "${module.logstash-kibana.logstash_dns}"
    "kibana"        = "${module.kibana.kibana_dns}"
  }
}
