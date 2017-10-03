ssh-key:
	ssh-keygen -q -N "" -b 4096 -C "test" -f ./id_rsa

plan:
	terraform-0.9.8 plan -target=module.vpc.module.public-subnets -target=module.vpc.module.private-subnets
	terraform-0.9.8 plan -target=module.vpc.module.public-gateway -target=module.vpc.module.nat-gateway

apply:
	terraform-0.9.8 apply -target=module.vpc.module.public-subnets -target=module.vpc.module.private-subnets
	terraform-0.9.8 apply -target=module.vpc.module.public-gateway -target=module.vpc.module.nat-gateway
	terraform-0.9.8 apply

test:
	curl -v http://$(terraform-0.9.8 output elb_dns)

destroy:
	terraform-0.9.8 destroy -force
