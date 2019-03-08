provider "aws" {
  region = "us-east-1"
}

data "aws_availability_zones" "all" {}

resource "aws_key_pair" "sibi_fpco_key" {
  key_name   = "sibi_asg_key"
  public_key = "${file("id_rsa.pub")}"
}

# Creating EC2 instance
resource "aws_instance" "web" {
  ami                    = "ami-2757f631"
  count                  = "${var.count}"
  instance_type          = "t2.micro"
  vpc_security_group_ids = ["${aws_security_group.instance.id}"]
  key_name               = "${aws_key_pair.sibi_fpco_key.key_name}"
  tags {
    Name = "hello-world-web"
  }
}

# Creating an Monitor EC2 instance
resource "aws_instance" "monitor" {
  ami                    = "ami-2757f631"
  count                  = "${var.count}"
  instance_type          = "t2.micro"
  vpc_security_group_ids = ["${aws_security_group.instance.id}"]
  key_name               = "${aws_key_pair.sibi_fpco_key.key_name}"
  tags {
    Name = "monitor-ec2"
  }
}

### Creating Security Group for EC2
resource "aws_security_group" "instance" {
  name = "terraform-example-instance"

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

## Creating Launch Configuration
resource "aws_launch_configuration" "example" {
  image_id        = "ami-2757f631"
  instance_type   = "t2.micro"
  security_groups = ["${aws_security_group.instance.id}"]
  key_name        = "${aws_key_pair.sibi_fpco_key.key_name}"
  user_data       = "${file("template/user_data.sh")}"

  lifecycle {
    create_before_destroy = true
  }
}

## Creating AutoScaling Group
resource "aws_autoscaling_group" "example" {
  launch_configuration = "${aws_launch_configuration.example.id}"
  availability_zones   = ["${data.aws_availability_zones.all.names}"]
  min_size             = 2
  max_size             = 10
  load_balancers       = ["${aws_elb.example.name}"]
  health_check_type    = "ELB"

  tag {
    key                 = "Name"
    value               = "terraform-asg-example"
    propagate_at_launch = true
  }
}

## Security Group for ELB
resource "aws_security_group" "elb" {
  name = "terraform-example-elb"

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

### Creating ELB
resource "aws_elb" "example" {
  name               = "terraform-asg-example"
  security_groups    = ["${aws_security_group.elb.id}"]
  availability_zones = ["${data.aws_availability_zones.all.names}"]

  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 3
    interval            = 30
    target              = "HTTP:80/"
  }

  listener {
    lb_port           = 80
    lb_protocol       = "http"
    instance_port     = "80"
    instance_protocol = "http"
  }
}
