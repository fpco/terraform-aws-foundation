variable "name_prefix" {
  description = "prefix to use when naming resources in this kube stack (cluster)"
  default     = ""
  type        = string
}

variable "availability_zones" {
  description = "list of availability zones to deploy to"
  type        = list(string)
}

variable "key_name" {
  description = "name of aws_key_pair to use"
  type        = string
}

# load-balancer specific

variable "lb_subnet_ids" {
  description = "list of IDs of the subnets to deploy load-balancer (ELB) into"
  type        = list(string)
}

variable "lb_security_group_ids" {
  description = "list of security group IDs to associate with the load-balancer (ELB)"
  type        = list(string)
}

variable "private_load_balancer" {
  description = "boolean that makes to the load-balancer's (ELB's) `internal` attribute"
  default     = true
  type        = string
}

# controller-specific

variable "controller_ami" {
  description = "CoreOS AMI to use for controller nodes"
  type        = string
}

variable "controller_instance_type" {
  description = "instance types to use for controller nodes"
  default     = "t2.medium"
  type        = string
}

variable "controller_root_volume_type" {
  description = "standard, gp2, or io1"
  default     = "gp2"
  type        = string
}

variable "controller_root_volume_size" {
  description = "EBS volume size, in GB"
  default     = "20"
  type        = string
}

variable "controller_subnet_ids" {
  description = "list of IDs of the subnets to deploy controller nodes into"
  type        = list(string)
}

variable "controller_max_nodes" {
  description = ""
  default     = "5"
  type        = string
}

variable "controller_min_nodes" {
  description = "minimum number of nodes in controller ASG"
  default     = "3"
  type        = string
}

variable "controller_iam_profile" {
  description = "name of the IAM profile to assign to the controllers"
  type        = string
}

variable "controller_name_suffix" {
  description = ""
  default     = "kube-controller"
  type        = string
}

variable "controller_public_ip" {
  description = "enable/disable public IPs for the nodes in the controller ASG"
  default     = false
  type        = string
}

variable "controller_security_group_ids" {
  description = "list of security group IDs to associate with the controller nodes"
  type        = list(string)
}

variable "controller_extra_tags" {
  description = "list of tag maps to append to the controller nodes"
  default     = []
  type        = list(string)
}

# worker-specific

variable "worker_root_volume_type" {
  description = "standard, gp2, or io1"
  default     = "gp2"
  type        = string
}

variable "worker_root_volume_size" {
  description = "EBS volume size, in GB"
  default     = "20"
  type        = string
}

variable "worker_subnet_ids" {
  description = "list of IDs of the subnets to deploy worker nodes into"
  type        = list(string)
}

variable "worker_max_nodes" {
  description = ""
  default     = "4"
  type        = string
}

variable "worker_min_nodes" {
  description = "minimum number of nodes in worker ASG"
  default     = "2"
  type        = string
}

variable "worker_iam_profile" {
  description = "name of the IAM profile to assign to the workers"
  type        = string
}

variable "worker_name_suffix" {
  description = ""
  default     = "kube-worker"
  type        = string
}

variable "worker_public_ip" {
  description = "enable/disable public IPs for the nodes in the worker ASG"
  default     = false
  type        = string
}

variable "worker_security_group_ids" {
  description = "list of security group IDs to associate with the worker nodes"
  type        = list(string)
}

variable "worker_ami" {
  description = "CoreOS AMI to use for worker nodes"
  type        = string
}

variable "worker_instance_type" {
  description = "instance types to use for worker nodes"
  default     = "t2.medium"
  type        = string
}

variable "worker_extra_tags" {
  description = "list of tag maps to append to the worker nodes"
  default     = []
  type        = list(string)
}

