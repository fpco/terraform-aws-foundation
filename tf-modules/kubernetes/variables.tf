variable "fqdn" {}

variable "master_subnet_id" {}

variable "nodes_subnet_ids" {
  type = "list"
}

variable "nodes_max" {}

variable "nodes_min" {}

variable "nodes_desired" {}

variable "region" {}

variable "kops_state_store" {}

variable "ec2_pubkey" {}

variable "master_ami" {}

variable "master_instance_type" {}

variable "master_volume_size" {}

variable "nodeup_url" {}

variable "protokube_image_source" {}

variable "kube_assets" {}

variable "nodes_ami" {}

variable "nodes_instance_type" {}

variable "nodes_volume_size" {}

variable "vpc_id" {}
