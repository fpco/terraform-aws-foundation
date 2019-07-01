variable "name_prefix" {
  type        = string
  description = "The name of this auto-scaling cluster, this should be unique"
}

variable "name_suffix" {
  default     = "k8s-master"
  description = "The suffix to the name of this auto-scaling cluster"
  type        = string
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
  type        = string
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type        = string
}

variable "iam_profile" {
  default     = ""
  description = "The IAM profile to associate with AWS instances in the ASG"
  type        = string
}

variable "instance_type" {
  default     = "t2.medium"
  description = "The type (size) of AWS instance"
  type        = string
}

variable "placement_group" {
  default     = ""
  description = "The `id` of the `aws_placement_group` to associate with the ASG"
  type        = string
}

variable "user_data" {
  default     = "#!/bin/bash"
  description = "The user_data string to pass to cloud-init"
  type        = string
}

variable "az" {
  description = "the availability zone to deploy the kube controller to"
  type        = string
}

variable "subnet_id" {
  description = "The ID of the subnet to deploy the kube controller to"
  type        = string
}

variable "security_group_ids" {
  description = "list of security groups to associate with the ASG (by id)"
  type        = list(string)
}

variable "elb_names" {
  default     = []
  description = "list of load balancers to associate with the ASG (by name)"
  type        = list(string)
}

variable "root_volume_type" {
  default     = "gp2"
  description = "The type of EBS volume to use for the root block device"
  type        = string
}

variable "root_volume_size" {
  default     = "120"
  description = "The size of the EBS volume (in GB) for the root block device"
  type        = string
}

// List of maps, as extra tags to append to the Auto-Scaling Group
variable "extra_tags" {
  default     = []
  description = "Extra tags that will be added to ASG, as a list of maps"
  type        = list(object({key=string, value=string, propagate_at_launch=bool}))
  # see the example in this TF doc for more info:
  # https://www.terraform.io/docs/providers/aws/r/autoscaling_group.html
  # should be provided in the following form:
  #   list(
  #     map("key", "k1", "value", "value1", "propagate_at_launch", true),
  #     map("key", "k2", "value", "value2", "propagate_at_launch", true)
  #   ),
}

