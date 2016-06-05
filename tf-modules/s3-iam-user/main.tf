resource "aws_iam_user" "s3-user" {
    name = "${var.name}"
    path = "/"
}
resource "aws_iam_policy" "s3-user" {
    name = "${var.name}_s3_bucket_access"
    policy = <<EOF
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:ListBucket"
            ],
            "Resource": ["${join("\",\"",formatlist("arn:aws:s3:::%s",split(",", var.bucket_names)))}"]
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:ListObjects",
                "s3:PutObject",
                "s3:GetObject",
                "s3:CreateMultipartUpload"
            ],
            "Resource": ["${join("\",\"",formatlist("arn:aws:s3:::%s/*",split(",", var.bucket_names)))}"]
        }
    ]
}
EOF
}
resource "aws_iam_policy_attachment" "s3-user" {
    name = "${var.name}_s3_bucket_access"
    users = ["${aws_iam_user.s3-user.name}"]
    groups = []
    policy_arn = "${aws_iam_policy.s3-user.arn}"
}
output "iam_user" {
    value = "${aws_iam_user.s3-user.name}"
}
provider "aws" {
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
}

variable "name" {
    description = "the name of the user"
}
variable "bucket_names" {
    description = "the name of the bucket to grant access to"
}
variable "access_key" {}
variable "secret_key" {}
variable "region" {}
