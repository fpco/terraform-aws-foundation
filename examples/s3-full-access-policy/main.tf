variable "name" {
  description = "The name of policy that will be created."
  default     = "s3-full-access-example-policy"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-west-2"
}

variable "bucket_names" {
  description = "list of bucket names to grant access to"
  default     = ["s3-full-access-policy-bucket"]
}

variable "user_name" {
  description = "Username for the IAM user that will be created with full access to the newly created S3 bucket."
  default     = "full-access-test-user"
}

provider "aws" {
  region = var.region
}

module "s3-full-access-policy" {
  source       = "../../modules/s3-full-access-policy"
  name         = var.name
  bucket_names = var.bucket_names
}

# Provides an IAM user which will eventually receive full access.
resource "aws_iam_user" "s3-full-access-policy-user" {
  name = "${var.user_name}-full-access"
}

# Provides an IAM access key for the full access IAM user.
# This is a set of credentials that allow API requests to be made as an IAM user.
resource "aws_iam_access_key" "full-access-user-access-key" {
  user = aws_iam_user.s3-full-access-policy-user.name
}

# Provides an IAM user which will not receive access.
resource "aws_iam_user" "s3-no-access-policy-user" {
  name = "${var.user_name}-no-access"
}

# Provides an IAM access key for the IAM user with no access.
resource "aws_iam_access_key" "no-access-user-access-key" {
  user = aws_iam_user.s3-no-access-policy-user.name
}

# Attaches a Managed IAM Policy to an IAM user
resource "aws_iam_user_policy_attachment" "s3-full-access-policy-attachment" {
  user       = aws_iam_user.s3-full-access-policy-user.name
  policy_arn = module.s3-full-access-policy.arn
}

resource "aws_s3_bucket" "test-bucket" {
  bucket = var.bucket_names[0]
  region = var.region
  acl    = "private"

  tags = {
    Name = "Test bucket for s3-full-access-policy module"
  }
}

