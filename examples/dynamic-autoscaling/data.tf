data "aws_availability_zones" "available" {}

module "ami-ubuntu" {
  source  = "../../modules/ami-ubuntu"
  version = "16.04"
}
