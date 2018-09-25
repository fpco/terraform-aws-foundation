resource "aws_s3_bucket" "kops-state-bucket" {
  bucket = "${var.kubernetes_cluster_name}"
  region = "${var.aws_region}"
  acl    = "private"

  tags {
    Name = "Bucket for kops to store its state in"
  }
}
