//`arn` exported from `aws_s3_bucket`
output "arn" {
  value = "${aws_s3_bucket.s3.arn}"
}

//`id` exported from `aws_s3_bucket`
output "id" {
  value = "${aws_s3_bucket.s3.id}"
}

//`region` exported from `aws_s3_bucket`
output "region" {
  value = "${aws_s3_bucket.s3.region}"
}

//`hosted_zone` exported from `aws_s3_bucket`
output "hosted_zone_id" {
  value = "${aws_s3_bucket.s3.hosted_zone_id}"
}

//`id` exported from `aws_iam_access_key`
output "aws_access_key_id" {
  value = "${aws_iam_access_key.s3.id}"
}

//`secret` exported from `aws_iam_access_key`
output "aws_secret_access_key" {
  value = "${aws_iam_access_key.s3.secret}"
}
