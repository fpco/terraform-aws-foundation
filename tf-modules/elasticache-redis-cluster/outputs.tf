//`id`, exported from `aws_subnet` A
output "subnet_a_id" {
  value = "${aws_subnet.a.id}"
}

//`id`, exported from `aws_subnet` C
output "subnet_c_id" {
  value = "${aws_subnet.a.id}"
}

//`redis_url`, exported from the `aws_elasticache_cluster`
output "redis_url" {
  value = "${aws_elasticache_cluster.redis.cache_nodes.0.address}"
}
