provider "aws" {
  region = "us-west-2"
}

data "aws_availability_zones" "azs" {
  state = "available"
}

module "vpc" {
  source               = "fpco/foundation/aws//modules/vpc-scenario-2"
  cidr                 = "10.0.0.0/16"
  public_subnet_cidrs  = ["10.0.0.0/24"]
  private_subnet_cidrs = ["10.0.1.0/24", "10.0.2.0/24"]
  region               = "us-west-2"
  azs                  = data.aws_availability_zones.azs.names
  name_prefix          = "rds-poc"
}

module "rds" {
  source            = "../../modules/rds"
  name_prefix       = "rds-poc"
  subnet_ids        = module.vpc.private_subnet_ids
  db_storage_size   = 50
  db_storage_type   = "gp2"
  db_engine         = "postgres"
  db_instance_type  = "db.m4.large"
  security_group_id = aws_security_group.access-pgsql.id
  db_name              = "db"
  db_username          = "user"
  db_password          = "Pass"
}

resource "aws_security_group" "access-pgsql" {
  vpc_id = module.vpc.vpc_id
  ingress {
    from_port   = 5432
    to_port     = 5432
    protocol    = "tcp"
    cidr_blocks = ["10.0.0.0/16"]
  }
}
