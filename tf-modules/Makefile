.PHONY: help

.DEFAULT_GOAL = help

require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## Release a new version of a module. MODULE=subnets make release
release: require-MODULE
	@mkdir -p out
	$(eval VERSION := $(shell cat ${MODULE}/VERSION ))
	@echo Releasing: ${MODULE}-${VERSION}
	@cd ${MODULE} && tar czvf ../out/${MODULE}-${VERSION}.tar.gz .
	@aws s3 cp out/${MODULE}-${VERSION}.tar.gz s3://tfmodules/${MODULE}-${VERSION}.tar.gz --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers


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

