# Requester's side of the connection.
resource "aws_vpc_peering_connection" "peer" {
  vpc_id        = module.vpc1-ad.vpc_id
  peer_vpc_id   = module.vpc2.vpc_id
  peer_owner_id = data.aws_caller_identity.requester.account_id
  auto_accept   = true

  # tags = {
  #   Side = "Requester"
  # }

  requester {
    allow_remote_vpc_dns_resolution = true
  }

  accepter {
    allow_remote_vpc_dns_resolution = true    
  }
}

# Accepter's side of the connection.
resource "aws_vpc_peering_connection_accepter" "apeer" {
  provider                  = aws.peer
  vpc_peering_connection_id = aws_vpc_peering_connection.peer.id
  auto_accept               = true
  
  # tags = {
  #   Side = "Accepter"
  # }
}

# Route from vpc1 to vpc2
resource "aws_route" "vpc-peering-route-1-to-2" {
  route_table_id            = module.vpc1-ad.public_route_table_id
  destination_cidr_block    = var.vpc2_cidr
  vpc_peering_connection_id = aws_vpc_peering_connection.peer.id
}

# Route from vpc2 to vpc1
resource "aws_route" "vpc-peering-route-2-to-1" {
  route_table_id            = module.vpc2.public_route_table_id
  destination_cidr_block    = var.vpc1_cidr
  vpc_peering_connection_id = aws_vpc_peering_connection.peer.id
}

# Note that vpc1 also has private subnet.This creates a appropriate route for that.
resource "aws_route" "vpc-peering-route-1-to-2-ext" {
  route_table_id            = module.nat-gateway.route_table_ids[0]
  destination_cidr_block    = var.vpc2_cidr
  vpc_peering_connection_id = aws_vpc_peering_connection.peer.id
}
