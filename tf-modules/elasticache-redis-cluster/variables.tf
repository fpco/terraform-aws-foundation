variable "name" {
  default     = ""
  description = "The name of this elasticache cluster, this should be unique"
}

variable "instance_type" {
  default     = "cache.m3.medium"
  description = "See http://aws.amazon.com/elasticache/details/#Available_Cache_Node_Types"
}

variable "inbound_security_group" {
  default     = ""
  description = "The ID of the security group to use for incoming connections (to redis)"
}

variable "engine_version" {
  default     = "2.8.22"
  description = "See http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html"
}

variable "region" {
  description = "AWS region to deploy to"
  default     = ""
}

variable "vpc_id" {
  description = "The ID of the experimental VPC to deploy to, depends on the AWS region"
}

variable "route_table_id" {
  description = "The ID of the experimental routing table to use, depends on the AWS region"
}

variable "cidr_a" {
  default     = ""
  description = "The CIDR block for subnet a"
}

variable "cidr_c" {
  default     = ""
  description = "The CIDR block for subnet c"
}
