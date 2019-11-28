resource "aws_db_subnet_group" "rds_private_subnet" {
  subnet_ids = var.subnet_ids
}

resource "aws_db_instance" "default" {
  identifier_prefix       = var.name_prefix
  allocated_storage       = var.db_storage_size
  storage_type            = var.db_storage_type
  engine                  = var.db_engine
  engine_version          = var.engine_version
  instance_class          = var.db_instance_type
  db_subnet_group_name    = aws_db_subnet_group.rds_private_subnet.name
  multi_az                = var.multi_az
  backup_retention_period = var.backup_retention_period
  monitoring_interval     = var.monitoring_interval
  monitoring_role_arn     = aws_iam_role.rds_enhanced_monitoring.arn
  vpc_security_group_ids  = [var.security_group_id]
  name                    = var.db_name
  username                = var.db_username
  password                = var.db_password
  tags                    = var.tags
}
