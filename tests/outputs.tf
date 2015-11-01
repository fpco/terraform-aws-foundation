output "elasticache_url" {
    value = "${module.my-ec-cluster.redis_url}"
}
