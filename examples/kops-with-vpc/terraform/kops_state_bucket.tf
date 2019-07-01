module "kops-state-bucket" {
  source        = "../../../modules/s3-remote-state"
  region        = var.region
  bucket_name   = var.kubernetes_cluster_name
  force_destroy = true
}

