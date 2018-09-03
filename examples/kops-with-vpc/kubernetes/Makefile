.PHONY: kops-create-cluster kops-edit-cluster kops-update-cluster \
	kops-validate-cluster create-s3-bucket generate-ssh-keypair fetch-keys \
	kops-rolling-update

.DEFAULT_GOAL = help

export PASSWORD_STORE_DIR ?= ${PWD}/../password-store

require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## Fetch private/public ssh keys from pw store
fetch-keys: require-ENVIRONMENT require-CLUSTER_NAME
	@pass clusters/${CLUSTER_NAME}/keys/admin.pem.gpg > ${ENVIRONMENT}/${CLUSTER_NAME}-admin.pem
	@pass clusters/${CLUSTER_NAME}/keys/admin.pem.pub.gpg > ${ENVIRONMENT}/${CLUSTER_NAME}-admin.pem.pub

## Create s3 bucket for new cluster
create-s3-bucket: require-ENVIRONMENT require-CLUSTER_NAME
	@aws s3 mb s3://${CLUSTER_NAME}

## Create cluster with kops. Needs CLUSTER_NAME defined.
kops-create-cluster: require-ENVIRONMENT require-CLUSTER_NAME require-VPC_ID require-VPC_CIDR require-SSH_PUBLIC_KEY require-NODE_COUNT require-NODE_SIZE require-NODE_VOLUME_SIZE require-KUBERNETES_VERSION require-KOPS_MASTER_ZONES
	@kops create cluster \
		--cloud=aws \
		--kubernetes-version=${KUBERNETES_VERSION} \
		--authorization=RBAC \
		--networking="flannel" \
		--master-count=${KOPS_MASTER_COUNT} \
		--master-size=${KOPS_MASTER_SIZE} \
		--master-volume-size=${KOPS_MASTER_VOLUME_SIZE} \
		--master-zones=${KOPS_MASTER_ZONES} \
		--network-cidr=${VPC_CIDR} \
		--node-count=${NODE_COUNT} \
		--node-size=${NODE_SIZE} \
		--ssh-public-key=${ENVIRONMENT}/${SSH_PUBLIC_KEY} \
		--zones=us-east-2a,us-east-2b,us-east-2c \
		--vpc=${VPC_ID} \
		--node-volume-size=${NODE_VOLUME_SIZE} \
		--state=s3://${CLUSTER_NAME} \
		--name=${CLUSTER_NAME}

## Edit cluster info. Opens in default editor.
kops-edit-cluster: require-ENVIRONMENT require-CLUSTER_NAME
	@kops edit cluster --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME}

## Edit node instance group
kops-edit-igs-nodes: require-ENVIRONMENT require-CLUSTER_NAME
	@kops edit --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME} ig nodes

## DANGER: Delete cluster!!! OPTS=--yes
kops-delete-cluster: require-ENVIRONMENT require-CLUSTER_NAME
	@kops delete cluster --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME} ${OPTS}

## Update cluster. OPTS=--yes
kops-update-cluster: require-ENVIRONMENT require-CLUSTER_NAME
	@kops update cluster --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME} ${OPTS}

## Validate cluster
kops-validate-cluster: require-ENVIRONMENT require-CLUSTER_NAME
	@kops validate cluster --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME}

## Kops rolling update. OPTS=--force
kops-rolling-update: require-ENVIRONMENT require-CLUSTER_NAME
	@kops rolling-update cluster --state s3://${CLUSTER_NAME} --yes ${OPTS}

## Generate new ssh keypair.
generate-ssh-keypair: require-ENVIRONMENT require-CLUSTER_NAME
	@ssh-keygen -t rsa -b 4096 -f ${ENVIRONMENT}/${CLUSTER_NAME}-admin.pem -C "$$CLUSTER_NAME admin"

## Import keypair into aws
import-ssh-keypair: require-ENVIRONMENT require-CLUSTER_NAME
	@aws ec2 import-key-pair --key-name ${CLUSTER_NAME}-admin --region us-east-1 --public-key-material "`cat ${ENVIRONMENT}/${CLUSTER_NAME}-admin.pub)`"

## Fetch kubeconfig.yaml
fetch-kubeconfig: require-ENVIRONMENT require-CLUSTER_NAME
	@KUBECONFIG=${ENVIRONMENT}/kubeconfig.yaml kops export kubecfg --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
