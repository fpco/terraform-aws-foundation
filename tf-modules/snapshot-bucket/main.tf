/**
 * ## Snapshot Bucket
 *
 * **DEPRECATED: WILL BE REMOVED IN A FUTURE RELEASE**
 *
 * This module will create an S3 bucket with an associated IAM policy and access
 * key that provide access to that bucket.
 *
 */
resource "aws_iam_user" "s3" {
  name = "${var.name}"
  path = "/"
}

resource "aws_iam_access_key" "s3" {
  user = "${aws_iam_user.s3.name}"
}

resource "aws_iam_policy" "s3" {
  name = "${var.name}_all_access"

  policy = <<EOF
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:ListBucket"
            ],
            "Resource": [
                "arn:${var.aws_cloud}:s3:::${var.name}"
            ]
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:ListObjects",
                "s3:PutObject",
                "s3:GetObject",
                "s3:CreateMultipartUpload"
            ],
            "Resource": [
                "arn:${var.aws_cloud}:s3:::${var.name}/*"
            ]
        }
    ]
}
EOF
}

resource "aws_iam_policy_attachment" "s3" {
  name       = "${var.name}_all_access_attachment"
  users      = ["${aws_iam_user.s3.name}"]
  groups     = ["admin"]
  policy_arn = "${aws_iam_policy.s3.arn}"
}

resource "aws_s3_bucket" "s3" {
  depends_on = ["aws_iam_user.s3"]
  bucket     = "${var.name}"
  acl        = "private"

  tags {
    Name = "${var.name}"
  }
}
