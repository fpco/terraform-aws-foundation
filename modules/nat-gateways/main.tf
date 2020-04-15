/**
 * ## NAT Gateways
 *
 * Use `aws_eip` and `aws_nat_gateway` to create a NAT Gateway for each subnet
 * included in the `public_subnet_ids` variable. An `aws_route_table` is then
 * created for each NAT gateway and a `aws_route_table_association` to associate
 * the table with each of the subnets in the list of `private_subnet_ids`. Note
 * that is is OK to provide more private subnets than NAT gateways/public subnets.
 *
 */

locals {
  # Variable which controls if NAT and it's dependent resources has to be created or not
  total_nat_count = var.enable_nat_creation ? var.nat_count : 0
  # Number of new NATs to be created in case var.nat_eip is empty.
  total_new_nat   = var.enable_nat_creation ? (length(var.nat_eip) == 0 ? local.total_nat_count : 0) : 0
  # Gives the EIP ids. It would be either populated via data source or newly created (which is controlled by var.nat_eip).
  eip_ids         = var.enable_nat_creation ? (length(var.nat_eip) == 0 ? aws_eip.nat.*.id : values(data.aws_eip.nat)[*].id) : []
}

# AWS Managed NAT Gateways
resource "aws_eip" "nat" {
  count = local.total_new_nat
  vpc   = true
}

data "aws_eip" "nat" {
  for_each  = length(var.nat_eip) != 0 ? toset(var.nat_eip) : toset([])
  public_ip = each.value
}

data "aws_subnet" "public" {
  count = length(var.public_subnet_ids)
  id    = element(var.public_subnet_ids, count.index)
}

resource "aws_nat_gateway" "nat" {
  count         = local.total_nat_count
  subnet_id     = element(data.aws_subnet.public.*.id, count.index)
  allocation_id = element(local.eip_ids, count.index)

  tags = merge(
    {
      "Name" = "${var.name_prefix}-${format("%02d", count.index + 1)}"
    },
    var.extra_tags,
  )
}

# Route tables. One per NAT gateway.
resource "aws_route_table" "private" {
  count  = local.total_nat_count
  vpc_id = var.vpc_id

  tags = merge(
    {
      "Name" = "${var.name_prefix}-private-${format("%02d", count.index)}"
    },
    var.extra_tags,
  )
}

resource "aws_route" "private_nat_gateway" {
  count                  = local.total_nat_count
  route_table_id         = aws_route_table.private[count.index].id
  destination_cidr_block = "0.0.0.0/0"
  nat_gateway_id         = element(aws_nat_gateway.nat.*.id, count.index)
}

# https://github.com/terraform-providers/terraform-provider-aws/pull/6999
resource "aws_route_table_association" "private-rta" {
  count          = var.enable_nat_creation ? length(var.private_subnet_ids) : 0
  subnet_id      = element(var.private_subnet_ids, count.index)
  route_table_id = element(aws_route_table.private.*.id, count.index)
}
