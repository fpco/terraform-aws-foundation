output "subnet_a_id" {
    value = "${aws_subnet.a.id}"
}
  
output "subnet_c_id" {
    value = "${aws_subnet.a.id}"
}

output "redis_url" {
    value = "${aws_elasticache_cluster.redis.cache_nodes.0.address}"
}
