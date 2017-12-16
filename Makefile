# QUESTIONS:
#  * how to suppress the "make[1]: Entering directory '/builds/fpco-mirrors/fpco-terraform-aws'" messages?
#  * how best to reduce the code and variables here?
ARCHIVE_PATH := "${PWD}/archives"

.PHONY: help

.DEFAULT_GOAL = help

require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## rm -rf all zee archives
clean-archives: require-ARCHIVE_PATH
	@echo "rm -rf ${ARCHIVE_PATH}/"

## Build and release all stacks to the registry
release-all: release-all-stacks release-all-modules
	@echo "all done!"

## Build and release all stacks to the registry
release-all-stacks: require-ARCHIVE_PATH require-CHANNEL require-VERSION require-REGISTRY
	@echo "releasing all Stacks!"
	@for STACK in $$(ls tests) ; do\
		BASE_PATH=tests PREFIX=stacks MODULE=$$STACK make build-archive   ; \
		BASE_PATH=tests PREFIX=stacks MODULE=$$STACK make release-archive ; \
	done

## Build and release all modules to the registry
release-all-modules: require-ARCHIVE_PATH require-CHANNEL require-VERSION require-REGISTRY
	@echo "releasing all Modules!"
	@for M in $$(ls tf-modules) ; do \
		BASE_PATH=tf-modules PREFIX=modules MODULE=$$M make build-archive   ; \
		BASE_PATH=tf-modules PREFIX=modules MODULE=$$M make release-archive ; \
	done

## Build a new version of a module (or stack) as a .tgz artifact (do not release yet)
build-archive: require-ARCHIVE_PATH require-CHANNEL require-MODULE require-VERSION require-BASE_PATH
	@mkdir -p ${ARCHIVE_PATH}/${CHANNEL}/${VERSION}/${PREFIX}/${MODULE}
	@echo "TODO: render markdown docs to README.md"
	@echo Building archive: ${PREFIX}/${MODULE}-${VERSION}
	@cd ${BASE_PATH}/${MODULE} && tar czvf ${ARCHIVE_PATH}/${CHANNEL}/${VERSION}/${PREFIX}/${MODULE}/${MODULE}-${VERSION}.tgz .
	@echo built: ${ARCHIVE_PATH}/${CHANNEL}/${VERSION}/${PREFIX}/${MODULE}/${MODULE}-${VERSION}.tgz

## Release a new version of a specific archive (either module or stack)
release-archive: require-ARCHIVE_PATH require-CHANNEL require-MODULE require-VERSION require-REGISTRY
	@echo "Releasing archive ${PREFIX}/${MODULE}-${VERSION} to ${CHANNEL} channel from ${ARCHIVE_PATH}/${CHANNEL}/${VERSION}/${PREFIX}/${MODULE}/${MODULE}-${VERSION}.tgz"
	@aws s3 cp --acl=public-read ${ARCHIVE_PATH}/${CHANNEL}/${VERSION}/${PREFIX}/${MODULE}/${MODULE}-${VERSION}.tgz s3://${REGISTRY}/${CHANNEL}/${VERSION}/${PREFIX}/${MODULE}-${VERSION}.tgz

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

