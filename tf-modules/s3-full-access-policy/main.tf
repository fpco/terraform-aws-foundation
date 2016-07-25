variable "name" {
    description = "the name of the account or deployment to use with this policy"
}
variable "bucket_names" {
    description = "the name of the buckets to grant access to"
}
output "id" {
    value = "${aws_iam_policy.s3-full-access.id}"
}
output "arn" {
    value = "${aws_iam_policy.s3-full-access.arn}"
}
output "name" {
    value = "${aws_iam_policy.s3-full-access.name}"
}
resource "aws_iam_policy" "s3-full-access" {
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
