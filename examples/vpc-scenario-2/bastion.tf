resource "aws_instance" "bastion" {
  ami                         = module.ubuntu-xenial-ami.id
  associate_public_ip_address = "true"
  instance_type               = "t2.nano"
  key_name                    = aws_key_pair.main.key_name
  subnet_id                   = module.vpc.public_subnet_ids[0]
  vpc_security_group_ids      = [aws_security_group.bastion.id]

  root_block_device {
    volume_type = "gp2"
    volume_size = "20"
  }

  lifecycle {
    ignore_changes = [
      ami,
      user_data,
    ]
  }

  user_data = <<END_INIT
#!/bin/bash
echo "hello init"
END_INIT

}

# Security group for the bastion instance
resource "aws_security_group" "bastion" {
  name   = "${var.name}-bastion"
  vpc_id = module.vpc.vpc_id
}

module "bastion-ssh-rule" {
  source = "../../modules/ssh-sg"

  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.bastion.id
}

# open egress for bastion instance (outbound from the node)
module "bastion-egress-rule" {
  source = "../../modules/open-egress-sg"
  security_group_id = aws_security_group.bastion.id
}

# EIP for the bastion instance
resource "aws_eip" "bastion" {
  vpc = "true"
}

# Attach the EIP to the bastion instance
resource "aws_eip_association" "bastion" {
  allocation_id = aws_eip.bastion.id
  instance_id   = aws_instance.bastion.id
}
