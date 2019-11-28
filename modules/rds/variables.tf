variable "subnet_ids" {
  type        = list(string)
  description = "Subnets (should be private) to host RDS instances."
}

variable "name_prefix" {
  type = string
}

variable "db_storage_size" {
  type        = number
  description = "The allocated storage in gibibytes."
}

variable "db_storage_type" {
  type        = string
  description = "One of \"standard\" (magnetic), \"gp2\" (general purpose SSD), or \"io1\" (provisioned IOPS SSD)."
}

variable "db_engine" {
  type        = string
  description = "The database engine to use."
}

variable "db_instance_type" {
  type        = string
  description = "The instance type of the RDS instance."
}

variable "security_group_id" {
  type        = string
  description = "The security group grants the access to database."
}

variable "db_name" {
  type        = string
  description = "The name of the database to create when the DB instance is created."
}

variable "db_username" {
  type        = string
  description = "Username for the master DB user."
}

variable "db_password" {
  type        = string
  description = "Password for the master DB user."
}

variable "tags" {
  type    = map(string)
  default = {}
}

variable "engine_version" {
  type        = string
  description = "The engine version to use."
}

variable "backup_retention_period" {
  description = "The days to retain backups for."
  default     = 7
}

variable "monitoring_interval" {
  description = "The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance."
  default     = 30
}

variable "multi_az" {
  description = "Specifies if the RDS instance is multi-AZ"
  default     = true
}
