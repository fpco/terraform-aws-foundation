variable "name_prefix" {
  description = "prefix to use when naming resources in this kube stack (cluster)"
  default     = ""
}

variable "availability_zones" {
  description = "list of availability zones to deploy to"
  type        = "list"
}

variable "key_name" {
  description = "name of aws_key_pair to use"
}

# load-balancer specific

variable "lb_subnet_ids" {
  description = "list of IDs of the subnets to deploy load-balancer (ELB) into"
  type        = "list"
}

variable "lb_security_group_ids" {
  description = "list of security group IDs to associate with the load-balancer (ELB)"
  type        = "list"
}

variable "private_load_balancer" {
  description = "boolean that makes to the load-balancer's (ELB's) `internal` attribute"
  default     = "true"
}

# controller-specific

variable "controller_ami" {
  description = "CoreOS AMI to use for controller nodes"
}

variable "controller_instance_type" {
  description = "instance types to use for controller nodes"
  default     = "t2.medium"
}

variable "controller_root_volume_type" {
  description = "standard, gp2, or io1"
  default     = "gp2"
}

variable "controller_root_volume_size" {
  description = "EBS volume size, in GB"
  default     = "20"
}

variable "controller_subnet_ids" {
  description = "list of IDs of the subnets to deploy controller nodes into"
  type        = "list"
}

variable "controller_desired_capacity" {
  description = ""
  default     = "3"
}

variable "controller_max_nodes" {
  description = ""
  default     = "5"
}

variable "controller_min_nodes" {
  description = "minimum number of nodes in controller ASG"
  default     = "2"
}

variable "controller_iam_profile" {
  description = "name of the IAM profile to assign to the controllers"
}

variable "controller_name_suffix" {
  description = ""
  default     = "kube-controller"
}

variable "controller_public_ip" {
  description = "enable/disable public IPs for the nodes in the controller ASG"
  default     = "false"
}

variable "controller_security_group_ids" {
  description = "list of security group IDs to associate with the controller nodes"
  type        = "list"
}

variable "controller_extra_tags" {
  description = "list of tag maps to append to the controller nodes"
  default     = []
  type        = "list"
}

# worker-specific

variable "worker_root_volume_type" {
  description = "standard, gp2, or io1"
  default     = "gp2"
}

variable "worker_root_volume_size" {
  description = "EBS volume size, in GB"
  default     = "20"
}

variable "worker_subnet_ids" {
  description = "list of IDs of the subnets to deploy worker nodes into"
  type        = "list"
}

variable "worker_desired_capacity" {
  description = ""
  default     = "2"
}

variable "worker_max_nodes" {
  description = ""
  default     = "4"
}

variable "worker_min_nodes" {
  description = "minimum number of nodes in worker ASG"
  default     = "0"
}

variable "worker_iam_profile" {
  description = "name of the IAM profile to assign to the workers"
}

variable "worker_name_suffix" {
  description = ""
  default     = "kube-worker"
}

variable "worker_public_ip" {
  description = "enable/disable public IPs for the nodes in the worker ASG"
  default     = "false"
}

variable "worker_security_group_ids" {
  description = "list of security group IDs to associate with the worker nodes"
  type        = "list"
}

variable "worker_ami" {
  description = "CoreOS AMI to use for worker nodes"
}

variable "worker_instance_type" {
  description = "instance types to use for worker nodes"
  default     = "t2.medium"
}

variable "worker_extra_tags" {
  description = "list of tag maps to append to the worker nodes"
  default     = []
  type        = "list"
}
