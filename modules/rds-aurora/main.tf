variable "azs" {
  type        = list(string)
  description = "Availibility zones for cluster to use."
}

variable "db_name" {
  type        = string
  description = "Database name."
}

variable "db_password" {
  type        = string
  description = "Database master password."
}

variable "db_username" {
  type        = string
  description = "Database master username."
}

variable "engine" {
  type        = string
  description = "Database engine name."
}

variable "identifier" {
  type        = string
  description = "Cluster identifier."
}

variable "instance_class" {
  type        = string
  default     = "db.r4.large"
  description = "Database instance type."
}

variable "instance_count" {
  type        = number
  default     = 1
  description = "Number of database instances to run."
}

variable "sg_ids" {
  type        = list(string)
  description = "Security group ids that controls the access to database."
}

variable "subnet_ids" {
  type        = list(string)
  description = "At least two subnet ids for database to live in. Better be private subnets. Ref: https://forums.aws.amazon.com/thread.jspa?threadID=151938"
}

resource "aws_db_subnet_group" "rds_private_subnet" {
  subnet_ids = var.subnet_ids
}

resource "aws_rds_cluster" "rds_cluster" {
  cluster_identifier     = var.identifier
  engine                 = var.engine
  availability_zones     = var.azs
  database_name          = var.db_name
  master_username        = var.db_username
  master_password        = var.db_password
  vpc_security_group_ids = var.sg_ids
  db_subnet_group_name   = aws_db_subnet_group.rds_private_subnet.name
  skip_final_snapshot    = true
}

resource "aws_rds_cluster_instance" "rds_instance" {
  count                = var.instance_count
  identifier           = "${var.identifier}-${count.index}"
  cluster_identifier   = aws_rds_cluster.rds_cluster.id
  engine               = var.engine
  instance_class       = var.instance_class
  publicly_accessible  = false
  db_subnet_group_name = aws_db_subnet_group.rds_private_subnet.name
}

output "endpoint_ro" {
  value = aws_rds_cluster.rds_cluster.reader_endpoint
}

output "endpoint_rw" {
  value = aws_rds_cluster.rds_cluster.endpoint
}
