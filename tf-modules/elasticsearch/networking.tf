
data "aws_subnet" "public" {
  count  = "${length(var.public_subnet_ids)}"
  id     = "${var.public_subnet_ids[count.index]}"
  vpc_id = "${var.vpc_id}"
}

data "aws_subnet" "private" {
  count  = "${length(var.private_subnet_ids)}"
  id     = "${var.private_subnet_ids[count.index]}"
  vpc_id = "${var.vpc_id}"
}

resource "aws_security_group" "transport-sg" {
  name        = "${var.name_prefix}-elasticsearch-transport"
  vpc_id      = "${var.vpc_id}"
  description = "Allow Elasticsearch Transport TCP (9300) and everything outbound."

  tags {
    Name = "${var.name_prefix}-elasticsearch-transport"
  }

  ingress {
    from_port   = 9300
    to_port     = 9300
    protocol    = "tcp"
    cidr_blocks = ["${data.aws_subnet.private.*.cidr_block}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "elasticsearch-api-sg" {
  name        = "${var.name_prefix}-elasticsearch-api"
  vpc_id      = "${var.vpc_id}"
  description = "Allow HTTP API (9200) inbound from ELB security group."

  tags {
    Name = "${var.name_prefix}-elasticsearch-api"
  }

  ingress {
    from_port       = 9200
    to_port         = 9200
    protocol        = "tcp"
    security_groups = ["${aws_security_group.elasticsearch-elb-sg.id}"]
  }

  ingress {
    from_port       = 9201
    to_port         = 9201
    protocol        = "tcp"
    security_groups = ["${aws_security_group.elasticsearch-elb-sg.id}"]
  }
}

resource "aws_security_group" "elasticsearch-elb-sg" {
  name        = "${var.name_prefix}-elasticsearch-elb"
  vpc_id      = "${var.vpc_id}"
  description = "Allow HTTP API (9200) from private subnet and everything outbound."

  tags {
    Name = "${var.elasticsearch_dns_name}"
  }

  ingress {
    from_port   = 9200
    to_port     = 9200
    protocol    = "tcp"
    cidr_blocks = ["${data.aws_subnet.public.*.cidr_block}"]
  }

  ingress {
    from_port   = 9201
    to_port     = 9201
    protocol    = "tcp"
    cidr_blocks = ["${var.auth_elb_ingress_cidrs}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}
