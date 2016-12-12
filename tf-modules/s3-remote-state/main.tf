/**
 *## S3 Bucket to Store Remote State
 *
 * 
 */
variable "bucket_name" {
    description = "the name to give the bucket"
}
variable "iam_users" {
    default     = ""
    description = "comma separated string-list of user names, full access to bucket"
}
variable "iam_groups" {
    default     = ""
    description = "comma separated string-list of group names, full access to bucket"
}

resource "aws_s3_bucket" "remote-state" {
    bucket = "${var.bucket_name}"
    acl    = "private"
    versioning {
        enabled = true
    }
}
module "remote-state-full-access-policy" {
    source = "../s3-full-access-policy"
    name = "${var.bucket_name}-full-access"
    bucket_names = "${aws_s3_bucket.remote-state.id}"
}
resource "aws_iam_policy_attachment" "remote-state-admin" {
    name   = "${module.remote-state-full-access-policy.name}"
    users  = ["${compact(split(",", replace(var.iam_users,  " ", "")))}"]
    groups = ["${compact(split(",", replace(var.iam_groups, " ", "")))}"]
    policy_arn = "${module.remote-state-full-access-policy.arn}"
}
//`arn` exported from `aws_s3_bucket`
output "bucket_arn" {
    value = "${aws_s3_bucket.remote-state.arn}"
}
//`id` exported from `aws_s3_bucket`
output "bucket_id" {
    value = "${aws_s3_bucket.remote-state.id}"
}
//`region` exported from `aws_s3_bucket`
output "region" {
    value = "${aws_s3_bucket.remote-state.region}"
}
//`arn` exported from the "full access" IAM policy to the bucket
output "iam_policy_arn" {
    value = "${module.remote-state-full-access-policy.arn}"
}
//`name` exported from the "full access" IAM policy to the bucket
output "iam_policy_name" {
    value = "${module.remote-state-full-access-policy.name}"
}
//Derived URL to the S3 bucket
output "url" {
    value = "https://s3-${aws_s3_bucket.remote-state.region}.amazonaws.com/${aws_s3_bucket.remote-state.id}"
}
//Export `iam_users` variable (list of IAM users with access to the bucket)
output "iam_users" {
    value = "${var.iam_users}"
}
//Export `iam_groups` variable (list of IAM groups with access to the bucket)
output "iam_groups" {
    value = "${var.iam_groups}"
}
