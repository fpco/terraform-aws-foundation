/**
 * ## Redis on AWS (Elasticache)
 *
 * This module creates the few pieces for an elasticache cluster including:
 *
 * * the `aws_elasticache_cluster` resource
 * * an associated `aws_elasticache_subnet_group`
 * * two (private) subnets and the route table associations
 * * a security group for internal redis communications between the nodes
 *
 * NOTES: **should update to support N subnets, and to separate out the security group into its own module (called here)**
 *
 *
 * ### How to Use this Module
 *
 * ```
 * resource "aws_security_group" "redis-inbound" {
 *     name = "${var.name}-queue-redis-inbound"
 *     vpc_id = "${aws_vpc.my_vpc.id}"
 *     ingress {
 *         from_port = 6379
 *         to_port = 6379
 *         protocol = "tcp"
 *         cidr_blocks = [ "${var.cidr_minion_a}", "${var.cidr_minion_c}"]
 *     }
 *     tags {
 *         Description = "Allow redis-client from workers cluster"
 *     }
 * }
 *
 * module "my-ec-cluster" {
 *     source = "../tf-modules/elasticache-redis-cluster"
 *     name = "${var.name}-redis-ec"
 *     region = "${var.region}"
 *     cidr_a = "10.10.10.0/24"
 *     cidr_c = "10.10.11.0/24"
 *     route_table_id = "${aws_route_table.my_app.id}"
 *     vpc_id = "${aws_vpc.my_vpc.id}"
 *     inbound_security_group = "${aws_security_group.redis-inbound.id}"
 *     instance_type = "${var.instance_type.redis}"
 * }
 * ```
 *
 */
resource "aws_elasticache_cluster" "redis" {
  cluster_id      = "${var.name}"
  engine          = "redis"
  engine_version  = "${var.engine_version}"
  node_type       = "${var.instance_type}"
  port            = 6379
  num_cache_nodes = 1

  #   parameter_group_name = "default.memcached1.4"
  subnet_group_name = "${aws_elasticache_subnet_group.redis.name}"

  security_group_ids = ["${aws_security_group.redis-internal.id}",
    "${var.inbound_security_group}",
  ]
}

resource "aws_elasticache_subnet_group" "redis" {
  name        = "${var.name}-${var.region}"
  description = "Redis on Elasticache for ${var.name}"
  subnet_ids  = ["${aws_subnet.a.id}", "${aws_subnet.c.id}"]
}

# Subnets in each of two AZ in this region (a and c are the only two in all three US regions)
resource "aws_subnet" "a" {
  availability_zone       = "${var.region}a"
  cidr_block              = "${var.cidr_a}"
  map_public_ip_on_launch = false
  vpc_id                  = "${var.vpc_id}"

  tags {
    Name        = "${var.name}-redis-a-${var.region}"
    Description = "Elasticache subnet for ${var.name}"
  }
}

resource "aws_subnet" "c" {
  availability_zone       = "${var.region}c"
  cidr_block              = "${var.cidr_c}"
  map_public_ip_on_launch = false
  vpc_id                  = "${var.vpc_id}"

  tags {
    Name        = "${var.name}-redis-c-${var.region}"
    Description = "Elasticache subnet for ${var.name}"
  }
}

# Routing table association for each minion subnet to the VPC IGW
resource "aws_route_table_association" "a" {
  route_table_id = "${var.route_table_id}"
  subnet_id      = "${aws_subnet.a.id}"
}

resource "aws_route_table_association" "c" {
  route_table_id = "${var.route_table_id}"
  subnet_id      = "${aws_subnet.c.id}"
}

# Security group to allow communication from the specified networks
resource "aws_security_group" "redis-internal" {
  name   = "${var.name}-${var.region}-redis-internal"
  vpc_id = "${var.vpc_id}"

  tags {
    Description = "Allow redis to instances in the elasticache subnets for ${var.name}"
  }

  ingress {
    from_port   = 6379
    to_port     = 6379
    protocol    = "tcp"
    cidr_blocks = ["${aws_subnet.a.cidr_block}", "${aws_subnet.c.cidr_block}"]
  }
}
