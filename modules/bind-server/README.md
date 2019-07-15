## Simple DNS server(s) running `bind9`

Supports setting up multiple servers in different AZs using the same
configuration.

Currently the intent is for setting up caching/forwarding
servers on private VPCs (as Route53 provides serving DNS zones well).

The `null_resource` provisioner will upload new options
and reload the configuration whenever it changes.

TODO: support uploading additional files, such as DNS zones

### Example

    module "dns" {
      source  = "fpco/foundation/aws//modules/bind-server"

      name               = "dns"
      ami                = "ami-7c803d1c" #Ubuntu 16.04
      subnet_ids         = "${module.vpc.private_subnets}"
      private_ips        = ["${var.private_dns_ips}"]
      security_group_ids = ["${aws_security_group.dns.id}"]
      key_name           = "${aws_key_pair.admin.id}"
      named_conf_options = "${file("${path.module}/files/dns/named.conf.options")}"
    }

And `files/dns/named.conf.options` would be something like this:

    options {
    	directory "/var/cache/bind";
    	forward only;
      # This is the Amazon DNS server, which is always at the VPC's '+2' address
    	forwarders {
    	 	10.0.0.2;
    	};
    	dnssec-validation auto;

    	auth-nxdomain no;    # conform to RFC1035
    	listen-on-v6 { any; };

    	allow-query { any; };
    };
    # This is a private zone served by Route 53 that uses the Amazon DNS server
    zone "route53.example.com." {
    	type forward;
    	forward only;
    	forwarders { 10.0.0.2; };
    };
    # This is the corporate domain that forwards/caches the corporate DNS servers
    zone "example.com." {
    	type forward;
    	forwarders { 10.10.1.50; 10.10.1.51; };
    };

Note that the instance has auto-recovery enabled, so in case there is a
hardware failure, the instance will be recreated on a new hardware node (but
retaining the same instance ID and ip addresses).

