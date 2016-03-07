output "arn" {
    value = "${aws_s3_bucket.s3.arn}"
}
output "id" {
    value = "${aws_s3_bucket.s3.id}"
}
output "region" {
    value = "${aws_s3_bucket.s3.region}"
}
output "hosted_zone_id" {
    value = "${aws_s3_bucket.s3.hosted_zone_id}"
}
output "aws_access_key_id" {
    value = "${aws_iam_access_key.s3.id}"
}
output "aws_secret_access_key" {
    value = "${aws_iam_access_key.s3.secret}"
}
