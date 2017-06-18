resource "aws_security_group" "redis-inbound" {
    name = "${var.name}-queue-redis-inbound"
    vpc_id = "${module.test-vpc.id}"
    ingress {
        from_port = 6379
        to_port = 6379
        protocol = "tcp"
        cidr_blocks = [ "${var.cidr_minions_a}", "${var.cidr_minions_c}"]
    }
    tags {
        Description = "Allow redis-client from workers cluster"
    }
}

module "my-ec-cluster" {
    source = "../tf-modules/elasticache-redis-cluster"
    name = "${var.name}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    cidr_a = "${var.cidr_redis_a}"
    cidr_c = "${var.cidr_redis_c}"
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${module.test-vpc.route_table_id}"
    inbound_security_group = "${aws_security_group.redis-inbound.id}"
    instance_type = "cache.m3.medium"
}
