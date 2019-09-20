variable "name_prefix" {
  description = "Creates a unique name beginning with the specified prefix."
}

resource "aws_iam_instance_profile" "profile" {
  name_prefix = var.name_prefix
  role        = aws_iam_role.role.name
}

resource "aws_iam_role" "role" {
  name               = var.name_prefix
  path               = "/"
  assume_role_policy = <<-EOF
    {
      "Version": "2012-10-17",
      "Statement": [
        {
          "Action": "sts:AssumeRole",
          "Principal": {
            "Service": "ec2.amazonaws.com"
          },
          "Effect": "Allow",
          "Sid": ""
        }
      ]
    }
EOF

}

output "iam_role_name" {
  value = aws_iam_role.role.name
}

output "iam_role_arn" {
  value = aws_iam_role.role.arn
}

output "iam_role_id" {
  value = aws_iam_role.role.id
}

output "iam_profile_id" {
  value = aws_iam_instance_profile.profile.id
}

