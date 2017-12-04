variable "name_prefix" {
  type        = "string"
  description = "The name of this auto-scaling cluster, this should be unique"
}

variable "name_suffix" {
  default     = "cluster"
  description = "The suffix to the name of this auto-scaling cluster"
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
  type        = "string"
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type        = "string"
}

variable "iam_profile" {
  default     = ""
  description = "The IAM profile to associate with AWS instances in the ASG"
}

variable "instance_type" {
  default     = "t2.micro"
  description = "The type of AWS instance (size)"
}

variable "placement_group" {
  default     = ""
  description = "The `id` of the `aws_placement_group` to associate with the ASG"
}

variable "user_data" {
  default     = ""
  description = "The user_data string to pass to cloud-init"
}

variable "max_nodes" {
  description = "The maximum number of nodes in each group"
  type        = "string"
}

variable "min_nodes" {
  description = "The minimum number of nodes in each group"
  type        = "string"
}

variable "desired_capacity" {
  default     = ""
  description = "The desired number of nodes in each group"
}

variable "public_ip" {
  default     = "true"
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
}

variable "azs" {
  description = "list of availability zones to associate with the ASG"
  type        = "list"
}

variable "subnet_ids" {
  description = "list of subnets to associate with the ASG (by id)"
  type        = "list"
}

variable "security_group_ids" {
  description = "list of security groups to associate with the ASG (by id)"
  type        = "list"
}

variable "elb_names" {
  default     = []
  description = "list of load balancers to associate with the ASG (by name)"
}

variable "root_volume_type" {
  default     = "gp2"
  description = "The type of EBS volume to use for the root block device"
}

variable "root_volume_size" {
  default     = "15"
  description = "The size of the EBS volume (in GB) for the root block device"
}

// List of maps, as extra tags to append to the Auto-Scaling Group
variable "extra_tags" {
  description = "Extra tags that will be added to ASG, as a list of maps"
  default     = []

  # see the example in this TF doc for more info:
  # https://www.terraform.io/docs/providers/aws/r/autoscaling_group.html
  # should be provided in the following form:
  #   list(
  #     map("key", "k1", "value", "value1", "propagate_at_launch", true),
  #     map("key", "k2", "value", "value2", "propagate_at_launch", true)
  #   ),
}
