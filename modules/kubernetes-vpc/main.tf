/**
 * ## VPC for Kubernetes
 *
 * **DEPRECATED**
 *
 */
resource "aws_vpc" "main" {
  cidr_block           = "${var.cidr}"
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = {
    KubernetesCluster = "${var.kube_fqdn}"
    Name              = "${var.project}-${var.env}-vpc"
  }
}

resource "aws_internet_gateway" "main" {
  vpc_id = "${aws_vpc.main.id}"

  tags = {
    KubernetesCluster = "${var.kube_fqdn}"
    Name              = "${var.project}-${var.env}-igw"
  }
}

resource "aws_vpc_dhcp_options" "main" {
  domain_name         = "${var.region}.compute.internal"
  domain_name_servers = ["AmazonProvidedDNS"]

  tags = {
    KubernetesCluster = "${var.kube_fqdn}"
    Name              = "${var.kube_fqdn}"
  }
}

resource "aws_vpc_dhcp_options_association" "main" {
  vpc_id          = "${aws_vpc.main.id}"
  dhcp_options_id = "${aws_vpc_dhcp_options.main.id}"
}

resource "aws_subnet" "public" {
  vpc_id            = "${aws_vpc.main.id}"
  cidr_block        = "${var.public_subnets[count.index]}"
  availability_zone = "${var.azs[count.index]}"
  count             = "${length(var.public_subnets)}"

  tags = {
    KubernetesCluster = "${var.kube_fqdn}"
    Name              = "${var.project}-${var.env}-subnet-public-${element(var.azs, count.index)}"
  }
}

resource "aws_route_table" "public" {
  vpc_id = "${aws_vpc.main.id}"

  tags = {
    KubernetesCluster = "${var.kube_fqdn}"
    Name              = "${var.project}-${var.env}-rt-main"
  }
}

resource "aws_route" "public_internet_gateway" {
  route_table_id         = "${aws_route_table.public.id}"
  destination_cidr_block = "0.0.0.0/0"
  gateway_id             = "${aws_internet_gateway.main.id}"
}

resource "aws_route_table_association" "public" {
  count          = "${length(var.public_subnets)}"
  subnet_id      = "${element(aws_subnet.public.*.id, count.index)}"
  route_table_id = "${aws_route_table.public.id}"
}
