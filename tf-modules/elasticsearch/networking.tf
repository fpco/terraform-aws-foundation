
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
  description = "Allow HTTP API (9200) and Optionally HTTPS API (9201) inbound from ALB security group."

  tags {
    Name = "${var.name_prefix}-elasticsearch-api"
  }
}


// Rule that allows ingress to API on EC2 instances from ALB
resource "aws_security_group_rule" "elasticsearch-api-rule" {
  from_port                = 9200
  to_port                  = 9200
  protocol                 = "tcp"
  security_group_id        = "${aws_security_group.elasticsearch-api-sg.id}"
  source_security_group_id = "${var.internal_alb_security_group_id}"
}


// Rule that allows ingress to ALB from the private subnet only
resource "aws_security_group_rule" "internal-alb-rule" {
  type              = "ingress"
  from_port         = 9200
  to_port           = 9200
  protocol          = "tcp"
  cidr_blocks       = ["${data.aws_subnet.private.*.cidr_block}"]
  security_group_id = "${var.internal_alb_security_group_id}"
}


// Optional rule that allows ingress to API with BasicAuth on EC2 instances from ALB
resource "aws_security_group_rule" "elasticsearch-api-secured-rule" {
  count                    = "${length(var.external_alb_security_group_id) == 0 ? 0 : 1}"
  from_port                = 9201
  to_port                  = 9201
  protocol                 = "tcp"
  security_group_id        = "${aws_security_group.elasticsearch-api-sg.id}"
  source_security_group_id = "${var.external_alb_security_group_id}"
}


// Optional rule that allows ingress to external ALB from the outside
resource "aws_security_group_rule" "external-alb-rule" {
  count             = "${length(var.external_alb_security_group_id) == 0 ? 0 : 1}"
  type              = "ingress"
  from_port         = 9201
  to_port           = 9201
  protocol          = "tcp"
  cidr_blocks       = ["${var.external_alb_ingress_cidrs}"]
  security_group_id = "${var.external_alb_security_group_id}"
}


# resource "aws_security_group" "elasticsearch-alb-sg" {
#   name        = "${var.name_prefix}-elasticsearch-alb"
#   vpc_id      = "${var.vpc_id}"
#   description = "Allow HTTP API (9200) from private subnet and everything outbound."

#   tags {
#     Name = "${var.elasticsearch_dns_name}"
#   }

#   ingress {
#     from_port   = 9200
#     to_port     = 9200
#     protocol    = "tcp"
#     cidr_blocks = ["${data.aws_subnet.public.*.cidr_block}"]
#   }

#   ingress {
#     from_port   = 9201
#     to_port     = 9201
#     protocol    = "tcp"
#     cidr_blocks = ["${var.auth_alb_ingress_cidrs}"]
#   }

#   egress {
#     from_port   = 0
#     to_port     = 0
#     protocol    = "-1"
#     cidr_blocks = ["0.0.0.0/0"]
#   }
# }
