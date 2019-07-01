output "master_node_asg_names" {
  value = aws_autoscaling_group.master-node-asg.*.name
}

output "data_node_asg_names" {
  value = aws_autoscaling_group.data-node-asg.*.name
}

output "master_node_ebs_volume_ids" {
  value = module.master-node-ebs-volumes.volume_ids
}

output "data_node_ebs_volume_ids" {
  value = module.data-node-ebs-volumes.volume_ids
}

output "internal_lb" {
  value = {
    "dns_name" = element(
      coalescelist(
        aws_elb.elasticsearch-internal-elb.*.dns_name,
        [lookup(var.internal_alb, "dns_name", "")],
      ),
      0,
    )
    "zone_id" = element(
      coalescelist(
        aws_elb.elasticsearch-internal-elb.*.zone_id,
        [lookup(var.internal_alb, "zone_id", "")],
      ),
      0,
    )
    "security_group_id" = var.internal_alb["security_group_id"]
  }

  description = "Internal ELB related info. Will either be ELB or ALB info."
}

output "external_lb" {
  value = {
    "dns_name" = element(
      coalescelist(
        aws_elb.elasticsearch-external-elb.*.dns_name,
        [lookup(var.external_alb, "dns_name", "")],
      ),
      0,
    )
    "zone_id" = element(
      coalescelist(
        aws_elb.elasticsearch-external-elb.*.zone_id,
        [lookup(var.external_alb, "zone_id", "")],
      ),
      0,
    )
    "security_group_id" = lookup(var.external_alb, "security_group_id", "")
  }

  description = "External ELB related info. Will either be ELB or ALB info."
}

