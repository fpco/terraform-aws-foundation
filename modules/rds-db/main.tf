variable "name_prefix" {
  type        = string
  description = "Prefix for all resource names."
}

variable "subnet_ids" {
  type        = list(string)
  description = "Subnets that the database instances live in."
}

variable "db_instance_type" {
  type        = string
  description = "TFE database instance type."
  default     = "db.m4.large"
}

variable "db_name" {
  type        = string
  description = "Name of TFE database."
}
variable "db_username" {
  type        = string
  description = "Master username of TFE database."
}
variable "db_password" {
  type        = string
  description = "Master password of TFE database."
}

variable "engine" {
  type        = string
  description = "Database engine being used."
}

variable "engine_version" {
  type        = string
  description = "Engine version that matches the engine variable."
}

variable "storage_type" {
  default     = "gp2"
  description = "Storage type for the database."
  type        = string
}

variable "storage_size" {
  default     = 50
  description = "Storage size (GiB) for the database."
}

variable "security_group_ids" {
  type        = list(string)
  description = "Security group ids for accessing the database."
}

variable "multi_az" {
	default     = true
	description = "If instances should be run across multiple AZs."
}

variable "skip_final_snapshot" {
	default     = false
  description = "If a snapshot is required before destory."
}

resource "aws_db_subnet_group" "rds_private_subnet" {
  subnet_ids = var.subnet_ids
}

resource "aws_db_instance" "default" {
  identifier_prefix      = var.name_prefix
  allocated_storage      = var.storage_size
  storage_type           = var.storage_type
  engine                 = var.engine
  engine_version         = var.engine_version
  instance_class         = var.db_instance_type
  db_subnet_group_name   = aws_db_subnet_group.rds_private_subnet.name
  multi_az               = var.multi_az
  skip_final_snapshot    = var.skip_final_snapshot
  vpc_security_group_ids = var.security_group_ids
  name                   = var.db_name
  username               = var.db_username
  password               = var.db_password
}

output "endpoint" {
  value = aws_db_instance.default.endpoint
}
