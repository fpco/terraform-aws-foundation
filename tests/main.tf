module "ami-ubuntu" {
  source = "../modules/ami-ubuntu"
}

module "asg" {
  source = "../modules/asg"

  ami                = ""
  key_name           = ""
  max_nodes          = ""
  min_nodes          = ""
  name_prefix        = ""
  security_group_ids = []
  subnet_ids         = []
}

module "aws-ipsec-vpn" {
  source = "../modules/aws-ipsec-vpn"

  name             = ""
  remote_device_ip = ""
  static_routes    = []
  vpc_id           = ""
}

module "bind-server" {
  source = "../modules/bind-server"

  ami                = ""
  key_name           = ""
  private_ips        = []
  security_group_ids = []
  subnet_ids         = []
}

module "bind-server-sg" {
  source = "../modules/bind-server-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "ci-cache-s3" {
  source = "../modules/ci-cache-s3"

  pgp_key = ""
  prefix  = ""
}

module "cloudtrail" {
  source = "../modules/cloudtrail"

  name_prefix = ""
}

module "cloudwatch-auto-recover-existing-ec2" {
  source = "../modules/cloudwatch-auto-recover-existing-ec2"
}

module "cloudwatch-exporter-iam" {
  source = "../modules/cloudwatch-exporter-iam"

  kube_cluster_nodes_arn = ""
  name_prefix            = ""
}

module "cloudwatch-logs-s3-bucket" {
  source = "../modules/cloudwatch-logs-s3-bucket"

  name_prefix = ""
  principals  = []
}

module "cluster-network" {
  source = "../modules/cluster-network"

  route_table_id = ""
  vpc_id         = ""
}

module "consul-agent-generic-init" {
  source = "../modules/consul-agent-generic-init"

  consul_client_token = ""
  consul_secret_key   = ""
  datacenter          = ""
  leader_dns          = ""
}

module "consul-agent-sg" {
  source = "../modules/consul-agent-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "consul-cluster" {
  source = "../modules/consul-cluster"

  ami            = ""
  key_name       = ""
  name           = ""
  route_table_id = ""
  vpc_id         = ""
}

module "consul-demo-server" {
  source = "../modules/consul-demo-server"

  ami                = ""
  key_name           = ""
  name_prefix        = ""
  region             = ""
  security_group_ids = []
  subnet_id          = ""
}

module "consul-leaders" {
  source = "../modules/consul-leaders"

  ami                 = ""
  consul_master_token = ""
  consul_secret_key   = ""
  route_table_id      = ""
  vpc_id              = ""
}

module "consul-leader-sg" {
  source = "../modules/consul-leader-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "consul-leaders-generic-init" {
  source = "../modules/consul-leaders-generic-init"
}

module "consul-leader-wan-sg" {
  source = "../modules/consul-leader-wan-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "credstash-grant" {
  source = "../modules/credstash-grant"

  kms_key_arn = ""
  roles_arns  = []
  roles_count = ""
  roles_names = []
}

module "credstash-setup" {
  source = "../modules/credstash-setup"
}

module "cross-account-assume-role-policy" {
  source = "../modules/cross-account-assume-role-policy"

  policy_name = ""
}

module "cross-account-group" {
  source = "../modules/cross-account-group"

  account_ids    = []
  mfa_policy_arn = ""
  name           = ""
  users          = []
}

module "cross-account-role" {
  source = "../modules/cross-account-role"

  name = ""
}

module "dnsmasq-server" {
  source = "../modules/dnsmasq-server"

  ami                = ""
  key_name           = ""
  name_prefix        = ""
  private_ips        = ""
  security_group_ids = []
  subnet_ids         = []
}

module "ebs-snapshot-iam" {
  source = "../modules/ebs-snapshot-iam"
}

module "ec2-auto-recover-instances" {
  source = "../modules/ec2-auto-recover-instances"

  ami                = ""
  key_name           = ""
  name_prefix        = ""
  private_ips        = []
  security_group_ids = []
  subnet_ids         = []
}

module "ec2-nat-instances" {
  source = "../modules/ec2-nat-instances"

  key_name             = ""
  name_prefix          = ""
  private_subnet_cidrs = []
  public_subnet_ids    = []
  security_group_ids   = []
}

module "elasticache-redis-cluster" {
  source = "../modules/elasticache-redis-cluster"

  vpc_id         = ""
  route_table_id = ""
}

module "elasticsearch" {
  source = "../modules/elasticsearch"

  credstash_get_cmd           = ""
  credstash_install_snippet   = ""
  credstash_kms_key_arn       = ""
  credstash_reader_policy_arn = ""
  elasticsearch_dns_name      = ""
  elasticsearch_version       = ""
  internal_alb                = ""
  key_name                    = ""
  logstash_beats_address      = []
  node_ami                    = ""
  private_subnet_ids          = []
  public_subnet_ids           = []
  vpc_id                      = ""
}

module "elk-r53" {
  source = "../modules/elk-r53"

  elasticsearch_dns_name = ""
  elasticsearch_lb       = ""
  kibana_dns_name        = ""
  kibana_lb              = ""
  logstash_dns_name      = ""
  logstash_lb            = ""
  route53_zone_id        = ""
}

module "elk-stack" {
  source = "../modules/elk-stack"

  credstash_get_cmd           = ""
  credstash_install_snippet   = ""
  credstash_kms_key_arn       = ""
  credstash_put_cmd           = ""
  credstash_reader_policy_arn = ""
  elasticsearch_dns_name      = ""
  elasticsearch_internal_alb  = {}
  kibana_alb                  = ""
  logstash_dns_name           = ""
  private_subnet_ids          = []
  public_subnet_ids           = []
  vpc_id                      = ""
}

module "etcd-server-sg" {
  source = "../modules/etcd-server-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "external-dns-iam" {
  source = "../modules/external-dns-iam"

  kube_cluster_nodes_arn = ""
  name_prefix            = ""
}

module "ha-management-cluster" {
  source = "../modules/ha-management-cluster"

  ami                = ""
  cidr_a             = ""
  cidr_c             = ""
  key_name           = ""
  security_group_ids = []
  worker_cidr_blocks = []
}

module "iam-group" {
  source     = "../modules/iam-group"
  group_name = ""
  members    = []
}

module "iam-users" {
  source    = "../modules/iam-users"
  user_list = []
}

module "iam-instance-profile" {
  source      = "../modules/iam-instance-profile"
  name_prefix = ""
}

module "init-snippet-attach-ebs-volume" {
  source = "../modules/init-snippet-attach-ebs-volume"

  region    = ""
  volume_id = ""
}

module "init-snippet-config-consul-leader" {
  source = "../modules/init-snippet-config-consul-leader"

  datacenter = ""
  encrypt    = ""
}

module "init-snippet-config-upstart-consul" {
  source = "../modules/init-snippet-config-upstart-consul"
}

module "init-snippet-consul-agent" {
  source = "../modules/init-snippet-consul-agent"

  consul_client_token = ""
  consul_secret_key   = ""
  datacenter          = ""
  leader_dns          = ""
}

module "init-snippet-consul-leader" {
  source = "../modules/init-snippet-consul-leader"

  cidr_prefix_a = ""
  cidr_prefix_c = ""
  datacenter    = ""
}

module "init-snippet-consul-template" {
  source = "../modules/init-snippet-consul-template"

  consul_client_token = ""
}

module "init-snippet-curator" {
  source = "../modules/init-snippet-curator"
}

module "init-snippet-exec" {
  source = "../modules/init-snippet-exec"

  init = ""
}

module "init-snippet-gitlab-docker" {
  source = "../modules/init-snippet-gitlab-docker"

  gitlab_domain        = ""
  registry_bucket_name = ""
}

module "init-snippet-hostname" {
  source = "../modules/init-snippet-hostname"

  hostname_prefix = ""
}

module "init-snippet-hostname-simple" {
  source = "../modules/init-snippet-hostname-simple"

  hostname_prefix = ""
}

module "init-snippet-install-awscli" {
  source = "../modules/init-snippet-install-awscli"
}

module "init-snippet-install-consul" {
  source = "../modules/init-snippet-install-consul"
}

module "init-snippet-install-ops" {
  source = "../modules/init-snippet-install-ops"
}

module "init-snippet-nexus" {
  source = "../modules/init-snippet-nexus"
}

module "init-snippet-nomad-agent" {
  source = "../modules/init-snippet-nomad-agent"

  nomad_pillar = ""
}

module "init-snippet-prometheus" {
  source = "../modules/init-snippet-prometheus"

  prometheus_pillar = ""
}

module "init-snippet-write-bootstrap-pillar" {
  source = "../modules/init-snippet-write-bootstrap-pillar"

  pillar = ""
}

module "kibana" {
  source = "../modules/kibana"

  alb                       = ""
  ami                       = ""
  credstash_get_cmd         = ""
  credstash_install_snippet = ""
  elasticsearch_url         = ""
  key_name                  = ""
  kibana_version            = ""
  name_prefix               = ""
  private_subnet_ids        = []
  vpc_id                    = ""
}

module "kube-controller-iam" {
  source = "../modules/kube-controller-iam"
}

module "kube-controller-sg" {
  source = "../modules/kube-controller-sg"

  cidr_blocks_api  = ""
  cidr_blocks_etcd = ""
  cidr_blocks_ssh  = ""
  name_prefix      = ""
  vpc_id           = ""
}

module "kube-load-balancer-sg" {
  source = "../modules/kube-load-balancer-sg"

  name_prefix = ""
  vpc_id      = ""
}

module "kubernetes-vpc" {
  source = "../modules/kubernetes-vpc"

  azs            = ""
  cidr           = ""
  env            = ""
  kube_fqdn      = ""
  project        = ""
  public_subnets = []
  region         = ""
}

module "kube-stack" {
  source = "../modules/kube-stack"

  availability_zones            = ""
  controller_ami                = ""
  controller_iam_profile        = ""
  controller_security_group_ids = ""
  controller_subnet_ids         = ""
  key_name                      = ""
  lb_security_group_ids         = []
  lb_subnet_ids                 = ""
  worker_ami                    = ""
  worker_iam_profile            = ""
  worker_security_group_ids     = []
  worker_subnet_ids             = []
}

module "kube-worker-iam" {
  source = "../modules/kube-worker-iam"
}

module "kube-worker-sg" {
  source = "../modules/kube-worker-sg"

  cidr_blocks_open_ingress = ""
  cidr_blocks_ssh          = ""
  name_prefix              = ""
  vpc_id                   = ""
}

module "leader-dns" {
  source = "../modules/leader-dns"

  cidr_a = ""
  cidr_c = ""
  domain = ""
}

module "logstash" {
  source = "../modules/logstash"

  ami                         = ""
  credstash_get_cmd           = ""
  credstash_install_snippet   = ""
  credstash_kms_key_arn       = ""
  credstash_put_cmd           = ""
  credstash_reader_policy_arn = ""
  elasticsearch_url           = ""
  key_name                    = ""
  logstash_dns_name           = ""
  logstash_version            = ""
  private_subnet_ids          = []
  public_subnet_ids           = []
  vpc_id                      = ""
}

module "mongo-server-sg" {
  source = "../modules/mongo-server-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "multi-volumes" {
  source = "../modules/multi-volumes"

  availability_zone = ""
  instance_id       = ""
  name              = ""
  sizes             = []
}

module "nat-gateways" {
  source = "../modules/nat-gateways"

  name_prefix        = ""
  nat_count          = ""
  private_subnet_ids = []
  public_subnet_ids  = []
  vpc_id             = ""
}

module "nomad-agent-sg" {
  source = "../modules/nomad-agent-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "nomad-agent-worker-ports-sg" {
  source = "../modules/nomad-agent-worker-ports-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "nomad-server-sg" {
  source = "../modules/nomad-server-sg"

  security_group_id  = ""
  server_cidr_blocks = []
  worker_cidr_blocks = []
}

module "open-egress-sg" {
  source = "../modules/open-egress-sg"

  security_group_id = ""
}

module "open-ingress-sg" {
  source = "../modules/open-ingress-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "packer-vpc" {
  source = "../modules/packer-vpc"
}

module "persistent-ebs" {
  source = "../modules/persistent-ebs"

  az          = ""
  name_prefix = ""
}

module "persistent-ebs-volumes" {
  source = "../modules/persistent-ebs-volumes"

  azs          = ""
  name_prefix  = ""
  snapshot_ids = []
  volume_count = ""
}

module "ping-request-sg" {
  source = "../modules/ping-request-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "ping-respond-sg" {
  source = "../modules/ping-respond-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "prometheus-node-exporter-sg" {
  source = "../modules/prometheus-node-exporter-sg"

  cidr_blocks       = []
  security_group_id = ""
}

module "prometheus-server" {
  source = "../modules/prometheus-server"

  ami                = ""
  az                 = ""
  instance_type      = ""
  key_file           = ""
  key_name           = ""
  name               = ""
  region             = ""
  security_group_ids = []
  subnet_id          = ""
}

module "r53-subdomain" {
  source = "../modules/r53-subdomain"

  name           = ""
  parent_zone_id = ""
}

module "route-public" {
  source = "../modules/route-public"

  name_prefix       = ""
  public_subnet_ids = []
  vpc_id            = ""
}

module "s3-full-access-policy" {
  source = "../modules/s3-full-access-policy"

  bucket_names = []
  name         = ""
}

module "s3-remote-state" {
  source = "../modules/s3-remote-state"

  bucket_name = ""
}

module "scale-asg-iam-policy" {
  source = "../modules/scale-asg-iam-policy"

  name = ""
}

module "security-group-base" {
  source = "../modules/security-group-base"

  description = ""
  name        = ""
  vpc_id      = ""
}

module "setup-meta-infrastructure" {
  source = "../modules/setup-meta-infrastructure"
}

module "single-node-asg" {
  source = "../modules/single-node-asg"

  ami                = ""
  instance_type      = ""
  key_name           = ""
  name_prefix        = ""
  name_suffix        = ""
  region             = ""
  security_group_ids = []
  subnet_id          = ""
}

module "single-port-sg" {
  source = "../modules/single-port-sg"

  cidr_blocks       = ""
  description       = ""
  port              = ""
  security_group_id = ""
}

module "snapshot-bucket" {
  source = "../modules/snapshot-bucket"

  name = ""
}

module "ssh-sg" {
  source = "../modules/ssh-sg"

  security_group_id = ""
}

module "subnets" {
  source = "../modules/subnets"

  azs         = ""
  cidr_blocks = ""
  name_prefix = ""
  vpc_id      = ""
}

module "tf-cloud-credential" {
  source = "../modules/tf-cloud-credential"
  
  name_prefix    = ""
  organization   = ""
  iam_access_key = { id = "", secret = "" }
  region         = ""
}  

module "vpc" {
  source = "../modules/vpc"

  cidr        = ""
  name_prefix = ""
  region      = ""
}

module "vpc-network" {
  source = "../modules/vpc-network"

  name            = ""
  vpc_cidr_prefix = ""
}

module "vpc-scenario-1" {
  source = "../modules/vpc-scenario-1"

  azs                 = ""
  cidr                = ""
  name_prefix         = ""
  public_subnet_cidrs = []
  region              = ""
}

module "vpc-scenario-2" {
  source = "../modules/vpc-scenario-2"

  azs                 = ""
  cidr                = ""
  name_prefix         = ""
  public_subnet_cidrs = []
  region              = ""
}

module "vpc-scenario-3" {
  source = "../modules/vpc-scenario-3"

  azs                 = ""
  cidr                = ""
  name_prefix         = ""
  public_subnet_cidrs = []
  region              = ""
  vpn_remote_ip       = ""
  vpn_static_routes   = []
}

module "vpc-scenario-4" {
  source = "../modules/vpc-scenario-4"

  azs                 = ""
  cidr                = ""
  name_prefix         = ""
  public_subnet_cidrs = []
  region              = ""
  vpn_remote_ip       = ""
  vpn_static_routes   = []
}

module "vpn-gateway" {
  source = "../modules/vpn-gateway"

  private_key        = ""
  public_key         = ""
  route53_zone_id    = ""
  vpc_cidr           = ""
  vpc_id             = ""
  vpc_route_table_id = ""
  vpc_subnet_id      = ""
  vpn_password       = ""
  vpn_username       = ""
}

module "web-alb" {
  source = "../modules/web-alb"

  dns_name               = ""
  http_target_group_arn  = ""
  https_target_group_arn = ""
  subnet_ids             = []
  vpc_id                 = ""
}
