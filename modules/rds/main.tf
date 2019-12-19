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
  type = string
}

variable "db_username" {
  type = string
}

variable "db_password" {
  type = string
}

variable "tags" {
  type    = map(string)
  default = {}
}

variable "engine_version" {
  type = string
}

resource "aws_db_subnet_group" "rds_private_subnet" {
  subnet_ids = var.subnet_ids
}

resource "aws_db_instance" "default" {
  identifier_prefix      = var.name_prefix
  allocated_storage      = var.db_storage_size
  storage_type           = var.db_storage_type
  engine                 = var.db_engine
  engine_version         = var.engine_version
  instance_class         = var.db_instance_type
  db_subnet_group_name   = aws_db_subnet_group.rds_private_subnet.name
  multi_az               = true
  skip_final_snapshot    = true
  vpc_security_group_ids = [var.security_group_id]
  name                   = var.db_name
  username               = var.db_username
  password               = var.db_password
  tags                   = var.tags
}

output "endpoint" {
  value = aws_db_instance.default.endpoint
}

output "db_id" {
  value = aws_db_instance.default.id
}
