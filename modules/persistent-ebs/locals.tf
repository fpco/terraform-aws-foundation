locals {
  volume_default = {
    type        = "gp2"
    iops        = 0
    size        = 15
    encrypted   = true
    kms_key_id  = ""
    snapshot_id = ""
  }
  volumes_default_no_comp = [for x in var.volumes : merge(local.volume_default, x)]
  volumes_default = var.compatible_with_single_volume ? [{
    type        = var.volume_type,
    iops        = var.iops,
    size        = var.size,
    encrypted   = var.encrypted,
    kms_key_id  = var.kms_key_id,
    snapshot_id = var.snapshot_id,
    name        = "default"
  }] : local.volumes_default_no_comp
}
