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

output "bucket_arn" {
    value = "${aws_s3_bucket.remote-state.arn}"
}
output "bucket_id" {
    value = "${aws_s3_bucket.remote-state.id}"
}
output "region" {
    value = "${aws_s3_bucket.remote-state.region}"
}
output "iam_policy_arn" {
    value = "${module.remote-state-full-access-policy.arn}"
}
output "iam_policy_name" {
    value = "${module.remote-state-full-access-policy.name}"
}
output "url" {
    value = "https://s3-${aws_s3_bucket.remote-state.region}.amazonaws.com/${aws_s3_bucket.remote-state.id}"
}
output "iam_users" {
    value = "${var.iam_users}"
}
output "iam_groups" {
    value = "${var.iam_groups}"
}
