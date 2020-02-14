variable "name_prefix" {
  type        = string
  description = "The name of this auto-scaling cluster, this should be unique"
}

variable "name_suffix" {
  default     = "cluster"
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
  default     = "t2.micro"
  description = "The type of AWS instance (size)"
  type        = string
}

variable "placement_group" {
  default     = ""
  description = "The `id` of the `aws_placement_group` to associate with the ASG"
  type        = string
}

variable "user_data" {
  default     = ""
  description = "The user_data string to pass to cloud-init"
  type        = string
}

variable "max_nodes" {
  description = "The maximum number of nodes in each group"
  type        = string
}

variable "min_nodes" {
  description = "The minimum number of nodes in each group"
  type        = string
}

variable "public_ip" {
  default     = true
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
  type        = bool
}

variable "subnet_ids" {
  description = "list of subnets to associate with the ASG (by id)"
  type        = list(string)
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

variable "alb_target_group_arns" {
  default     = []
  description = "list of target_group for application load balancer to associate with the ASG (by ARN)"
  type        = list(string)
}

variable "root_volume_type" {
  default     = "gp2"
  description = "The type of EBS volume to use for the root block device"
  type        = string
}

variable "root_volume_size" {
  default     = "15"
  description = "The size of the EBS volume (in GB) for the root block device"
  type        = string
}

variable "root_encrypted" {
  default     = false
  description = "Whether the volume should be encrypted or not. (Default: false)."
  type        = bool
}

variable "additional_block_devices" {
  default     = []
  description = "Additional EBS volumes to use as instance block devices"
  type        = list(object({ device_name = string, volume_type = string, volume_size = string, encrypted = bool }))
}

// List of maps, as extra tags to append to the Auto-Scaling Group
variable "extra_tags" {
  default     = []
  description = "Extra tags that will be added to ASG, as a list of maps"
  type        = list(object({ key = string, value = string, propagate_at_launch = bool }))
  # see the example in this TF doc for more info:
  # https://www.terraform.io/docs/providers/aws/r/autoscaling_group.html
  # should be provided in the following form:
  #   list(
  #     map("key", "k1", "value", "value1", "propagate_at_launch", true),
  #     map("key", "k2", "value", "value2", "propagate_at_launch", true)
  #   ),
}

variable "termination_policies" {
  default     = []
  description = "A list of policies to decide how the instances in the auto scale group should be terminated. The allowed values are OldestInstance, NewestInstance, OldestLaunchConfiguration, ClosestToNextInstanceHour, Default."
  type        = list(string)
}

variable "health_check_type" {
  default     = "EC2"
  description = "\"EC2\" or \"ELB\". Controls how health checking is done."
  type        = string
}

variable "enable_launching_hook" {
  default     = false
  description = "Specifies if launching hook needs to be enabled for this ASG"
  type        = bool
}

variable "enable_terminating_hook" {
  default     = false
  description = "Specifies if terminating hook needs to be enabled for this ASG"
  type        = bool
}

variable "lifecycle_sns_topic_arn" {
  default     = ""
  description = "ARN for the SNS topic serving as a lifecycle hook notificaiton target"
  type        = string
}

variable "aws_role_arn" {
  default     = ""
  description = "ARN for the AWS Role (should allow access to lifecycle_sns_topic_arn)"
  type        = string
}

variable "protect_from_scale_in" {
  default     = false
  description = "Allows setting instance protection. The autoscaling group will not select instances with this setting for terminination during scale in events."
  type        = bool
}

variable "suspended_processes" {
  default     = []
  description = "A list of processes to suspend for the AutoScaling Group. The allowed values are Launch, Terminate, HealthCheck, ReplaceUnhealthy, AZRebalance, AlarmNotification, ScheduledActions, AddToLoadBalancer. Note that if you suspend either the Launch or Terminate process types, it can prevent your autoscaling group from functioning properly."
  type        = list(string)
}
