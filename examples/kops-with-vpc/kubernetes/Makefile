.PHONY: \
	kops-create-cluster \
	kops-edit-cluster \
	kops-replace-subnets \
	kops-update-cluster \
	kops-validate-cluster \
	kops-rolling-update \
	clean

.DEFAULT_GOAL = help

## Make sure we have the required variable set in the environment
require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## Create cluster with kops. Needs CLUSTER_NAME defined.
kops-create-cluster: require-ENVIRONMENT require-CLUSTER_NAME require-VPC_ID require-VPC_CIDR require-SSH_PUBLIC_KEY require-KOPS_NODE_COUNT require-KOPS_NODE_SIZE require-KOPS_NODE_VOLUME_SIZE require-KUBERNETES_VERSION require-KOPS_MASTER_ZONES require-KOPS_ZONES
	@kops create cluster \
		--cloud=aws \
		--kubernetes-version=${KUBERNETES_VERSION} \
		--authorization=RBAC \
		--networking="flannel" \
		--master-count=${KOPS_MASTER_COUNT} \
		--master-size=${KOPS_MASTER_SIZE} \
		--master-volume-size=${KOPS_MASTER_VOLUME_SIZE} \
		--master-zones="${KOPS_MASTER_ZONES}" \
		--network-cidr=${VPC_CIDR} \
		--node-count=${KOPS_NODE_COUNT} \
		--node-size=${KOPS_NODE_SIZE} \
		--ssh-public-key=${ENVIRONMENT}/${SSH_PUBLIC_KEY} \
		--zones="${KOPS_ZONES}" \
		--vpc=${VPC_ID} \
		--node-volume-size=${KOPS_NODE_VOLUME_SIZE} \
		--state=s3://${CLUSTER_NAME} \
		--name=${CLUSTER_NAME}

## Replace subnets in cluster with ones in ENV
kops-replace-subnets: require-KOPS_ZONES require-KOPS_SUBNET_IDS require-KOPS_SUBNET_CIDRS
	@kops get cluster -o json | jq \
		--arg azs $$KOPS_ZONES \
		--arg ids $$KOPS_SUBNET_IDS \
		--arg cidrs $$KOPS_SUBNET_CIDRS \
		'setpath(["spec", "subnets"]; (reduce (range($$azs | split(",") | length)) as $$ix ([]; [ .[], {id: ($$ids | split(",") | .[$$ix]), zone: ($$azs | split(",") | .[$$ix]), name: ($$azs | split(",") | .[$$ix]), cidr: ($$cidrs | split(",") | .[$$ix]), type: "Public" }])))' | kops replace -f -

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
	@kops rolling-update cluster --state s3://${CLUSTER_NAME} ${OPTS}

## Fetch kubeconfig.yaml
fetch-kubeconfig: require-ENVIRONMENT require-CLUSTER_NAME
	@KUBECONFIG=${ENVIRONMENT}/kubeconfig.yaml kops export kubecfg --name ${CLUSTER_NAME} --state s3://${CLUSTER_NAME}

## Clean up stale files
clean:
	@rm -f ${ENVIRONMENT}/kubeconfig*
	@rm -f ${ENVIRONMENT}/*.pem*

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
