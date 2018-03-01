## NAT Gateways

Use `aws_eip` and `aws_nat_gateway` to create a NAT Gateway for each subnets
included in the `public_subnet_ids` variable. An `aws_route_table` is then
created for each NAT gateway and a `aws_route_table_association` to associate
the table with each of the subnets in the `private_subnet_ids`.

**TODO: CHECK TO SEE IF THIS MODULE COULD EVER HAVE ISSUES WITH AZ/SUBNETS CROSSING BETWEEN THE PUBLIC/PRIVATE PAIRS**

