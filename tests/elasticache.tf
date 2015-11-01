resource "aws_security_group" "redis-inbound" {
    name = "${var.name}-queue-redis-inbound"
    vpc_id = "${aws_vpc.test.id}"
    ingress {
        from_port = 6379
        to_port = 6379
        protocol = "tcp"
        cidr_blocks = [ "${var.cidr_minion_a}", "${var.cidr_minion_c}"]
    }
    tags {
        Description = "Allow redis-client from workers cluster"
    }
}

module "my-ec-cluster" {
    source = "../tf-modules/elasticache-redis-cluster"
    name = "${var.name}-redis-ec"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    cidr_a = "${var.cidr_minion_a}"
    cidr_c = "${var.cidr_minion_c}"
    route_table_id = "${aws_route_table.test.id}"
    vpc_id = "${aws_vpc.test.id}"
    inbound_security_group = "${aws_security_group.redis-inbound.id}"
    instance_type = "cache.m3.medium"
}

output "elasticache_url" {
    value = "${module.my-ec-cluster.redis_url}"
}
