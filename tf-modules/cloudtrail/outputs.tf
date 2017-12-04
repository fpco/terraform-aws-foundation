output "cloudtrail_id" {
  value = "${aws_cloudtrail.cloudtrail.id}"
}

output "cloudtrail_home_region" {
  value = "${aws_cloudtrail.cloudtrail.home_region}"
}

output "cloudtrail_arn" {
  value = "${aws_cloudtrail.cloudtrail.arn}"
}

output "s3_bucket_id" {
  value = "${aws_s3_bucket.cloudtrail.id}"
}

output "s3_bucket_arn" {
  value = "${aws_s3_bucket.cloudtrail.arn}"
}
