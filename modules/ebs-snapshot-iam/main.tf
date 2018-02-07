/**
 * ## IAM for Managing EBS Snapshots
 *
 * This module creates an `aws_iam_user` and `aws_iam_user_policy` that provide
 * access to EBS volumes and managing their snapshots, specifically:
 *
 * ```
 * "Action": [
 *   "ec2:CreateSnapshot",
 *   "ec2:CreateTags",
 *   "ec2:DeleteSnapshot",
 *   "ec2:DescribeAvailabilityZones",
 *   "ec2:DescribeSnapshots",
 *   "ec2:DescribeTags",
 *   "ec2:DescribeVolumeAttribute",
 *   "ec2:DescribeVolumeStatus",
 *   "ec2:DescribeVolumes"
 * ],
 * "Effect": "Allow",
 * "Resource": [
 *   "*"
 * ]
 * ```
 * 
 * NOTE: It would be nice to provide this as a role policy that could be attached
 * to a role, more freely than locking into an `aws_iam_user`.
 *
 */

variable "name" {
  default     = "snap-ebs"
  description = "name IAM resources, should match service name"
}

resource "aws_iam_user" "snap-ebs" {
  name = "${var.name}"
}

resource "aws_iam_user_policy" "snap-ebs" {
  name = "${var.name}"
  user = "${aws_iam_user.snap-ebs.name}"

  policy = <<EOF
{
  "Statement": [
    {
      "Action": [
        "ec2:CreateSnapshot",
        "ec2:CreateTags",
        "ec2:DeleteSnapshot",
        "ec2:DescribeAvailabilityZones",
        "ec2:DescribeSnapshots",
        "ec2:DescribeTags",
        "ec2:DescribeVolumeAttribute",
        "ec2:DescribeVolumeStatus",
        "ec2:DescribeVolumes"
      ],
      "Effect": "Allow",
      "Resource": [
        "*"
      ]
    }
  ]
}
EOF
}

// ARN of the IAM user created for the EBS snapshots service
output "user_arn" {
  value = "${aws_iam_user.snap-ebs.arn}"
}
