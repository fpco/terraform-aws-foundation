## Gateway and Routing for Public Subnets

This module provides an `aws_internet_gateway` and route table to use that
gateway to access the public internet (`0.0.0.0/0`). The module also accepts
a list of IDs for public subnets that should use the routing table.
