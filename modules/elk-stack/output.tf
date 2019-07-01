output "ami" {
  value       = module.ubuntu-ami.id
  description = "AMI that was used for all EC2 instances in the stack"
}

output "ssh_sg_id" {
  value       = aws_security_group.ssh.*.id
  description = "Security group that allows SSH access"
}

output "elasticsearch_master_node_asg_names" {
  value = module.elasticsearch.master_node_asg_names
}

output "elasticsearch_data_node_asg_names" {
  value = module.elasticsearch.data_node_asg_names
}

output "elasticsearch_master_node_ebs_volume_ids" {
  value = module.elasticsearch.master_node_ebs_volume_ids
}

output "elasticsearch_data_node_ebs_volume_ids" {
  value = module.elasticsearch.data_node_ebs_volume_ids
}

output "elasticsearch_internal_lb" {
  value = module.elasticsearch.internal_lb
}

output "elasticsearch_external_lb" {
  value = module.elasticsearch.external_lb
}

output "kibana_lb" {
  value = module.kibana.lb
}

output "logstash_kibana_asg_name" {
  value = module.logstash-kibana.asg_name
}

output "logstash_lb" {
  value = module.logstash-kibana.elb
}

output "kibana_http_target_group_arn" {
  value = module.kibana.http_target_group_arn
}

output "kibana_https_target_group_arn" {
  value = module.kibana.https_target_group_arn
}

