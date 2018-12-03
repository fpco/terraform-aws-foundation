## Simple DNS server(s) running dnsmasq.

Supports setting up multiple servers in different AZs using the same
configuration.

The intent is for setting up dnsmasq for private zones associated with a VPC

The `null_resource` provisioner will upload new options and reload the
configuration whenever it changes.

### Example

    data "template_file" "dnsmasq_conf" {
      vars {
        vpc_amazon_dns_ip     = "${var.vpc_amazon_dns_ip}"
        private_dns_zone_name = "${var.private_dns_zone_name}"
        ns1                   = "${var.ns[0]}"
        ns2                   = "${var.ns[1]}"
      }
      template = <<END_CONF
    # DNSMASQ CONFIG
    #
    domain-needed
    bogus-priv
    expand-hosts
    domain=${private_dns_zone_name}
    local=/${private_dns_zone_name}/
    server=${vpc_amazon_dns_ip}
    server=${ns1}
    server=${ns2}
    END_CONF
    }
    
    # The DNS servers
    module "dns" {
      source = "fpco/foundation/aws//modules/dnsmasq-server"

      name_prefix         = "${var.name}"
      dnsmasq_conf        = "${data.template_file.dnsmasq_conf.rendered}"
      ami                 = "${data.aws_ami.ubuntu-xenial.id}"
      key_name            = "${aws_key_pair.main.id}"
      ssh_key             = "${var.ssh_key}"
     #bastion_host        = "${aws_instance.bastion.public_dns}"
     #bastion_user        = "ubuntu"
     #bastion_private_key = "${var.ssh_key}"
      subnet_ids          = ["${module.private-subnets.ids}"]
      private_ips         = ["${var.private_dns_ips}"]
      security_group_ids  = [
        "${module.dns-server-sg.id}",
        "${module.public-ssh-sg.id}",
        "${module.open-egress-sg.id}",
      ]
      user_data = <<END_INIT
    ufw allow 53
    echo "10.10.0.10 foobar.${var.private_dns_zone_name}" >> /etc/hosts
    END_INIT
      alarm_actions = []
    }

