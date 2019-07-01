variable "name_prefix" {
  description = "Name to prefix the resources with."
  type        = string
}

variable "kube_cluster_nodes_arn" {
  description = "ARN of the Kube Nodes. Will be used in the iam policy identifier."
  type        = string
}

