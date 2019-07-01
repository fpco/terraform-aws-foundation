/**
 * ## NAT Gateways
 *
 * Use `aws_eip` and `aws_nat_gateway` to create a NAT Gateway for each subnets
 * included in the `public_subnet_ids` variable. An `aws_route_table` is then
 * created for each NAT gateway and a `aws_route_table_association` to associate
 * the table with each of the subnets in the `private_subnet_ids`.
 *
 * **TODO: CHECK TO SEE IF THIS MODULE COULD EVER HAVE ISSUES WITH AZ/SUBNETS CROSSING BETWEEN THE PUBLIC/PRIVATE PAIRS**
 *
 */

# AWS Managed NAT Gateways
resource "aws_eip" "nat" {
  count = var.nat_count
  vpc   = true
}

data "aws_subnet" "public" {
  count = length(var.public_subnet_ids)
  id    = element(var.public_subnet_ids, count.index)
}

resource "aws_nat_gateway" "nat" {
  count         = var.nat_count
  subnet_id     = element(data.aws_subnet.public.*.id, count.index)
  allocation_id = element(aws_eip.nat.*.id, count.index)
}

# Route tables. One per NAT gateway.
resource "aws_route_table" "private" {
  count  = var.nat_count
  vpc_id = var.vpc_id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = element(aws_nat_gateway.nat.*.id, count.index)
  }

  tags = merge(
    {
      "Name" = "${var.name_prefix}-private-${format("%02d", count.index)}"
    },
    var.extra_tags,
  )
}

resource "aws_route_table_association" "private-rta" {
  count          = var.nat_count
  subnet_id      = element(var.private_subnet_ids, count.index)
  route_table_id = element(aws_route_table.private.*.id, count.index)
}

