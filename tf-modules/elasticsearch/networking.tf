
resource "aws_security_group" "coordinators-elb-sg" {
  name        = "${var.name_prefix}-coordinators-elb-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow ICMP, HTTP from private subnet and everything outbound."

  ingress {
    from_port   = 9200
    to_port     = 9200
    protocol    = "tcp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}


resource "aws_security_group" "master-node-sg" {
  name        = "${var.name_prefix}-master-node-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow SSH, ICMP, Elasticsearch TCP, Elasticsearch HTTP, and everything outbound."

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  ingress {
    from_port   = 9300
    to_port     = 9300
    protocol    = "tcp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "data-node-sg" {
  name        = "${var.name_prefix}-data-node-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow SSH, ICMP, Elasticsearch TCP, Elasticsearch HTTP, and everything outbound."

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  ingress {
    from_port   = 9200
    to_port     = 9200
    protocol    = "tcp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  ingress {
    from_port   = 9300
    to_port     = 9300
    protocol    = "tcp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["${var.vpc_private_subnet_cidrs}", "${var.vpc_public_subnet_cidrs}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}
