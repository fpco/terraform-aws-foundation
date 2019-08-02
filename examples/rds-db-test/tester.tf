module "vpc" {
  source               = "../../modules/vpc-scenario-2"
  cidr                 = "192.168.0.0/16"
  public_subnet_cidrs  = ["192.168.0.0/24"]
  private_subnet_cidrs = ["192.168.10.0/24", "192.168.11.0/24"]
  azs                  = ["us-east-2a", "us-east-2b", "us-east-2c"]
  region               = "us-east-2"
  name_prefix          = "rds-test"
}

resource "aws_security_group" "hole" {
  vpc_id = "${module.vpc.vpc_id}"
  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

module "rds" {
  source         = "../../modules/rds-db"
  db_name        = "test"
  db_password    = "Pass!234"
  name_prefix    = "test"
  security_group_ids         = [aws_security_group.hole.id]
  db_username    = "master"
  engine         = "postgres"
  engine_version = "9.6.9"
  subnet_ids     = module.vpc.private_subnet_ids
}

output "endpoint" {
  value = module.rds.endpoint
}
