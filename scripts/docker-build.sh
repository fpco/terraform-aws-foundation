#!/usr/bin/env bash

set -o pipefail
set -o nounset
set -x

# WORKSPACE must be defined
if [ -z "${WORKSPACE-}" ]; then
  echo "ERROR: please define \$WORKSPACE , where git repos are checked out"
  exit 1
fi

TF_DOC_DIR=$WORKSPACE/fpco-terraform-aws
# need to be in the project repo to get git rev
cd $TF_DOC_DIR
GIT_REV=$(git rev-parse HEAD)

# if there is no BRANCH_NAME
if [ -z "${BRANCH_NAME-}" ]; then
  # check if there's a GIT_BRANCH
  if [ -z "${GIT_BRANCH-}" ]; then
    # figure out our own if there is neither
    BRANCH_NAME=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')
  fi
  # use GIT_BRANCH if it exists (and BRANCH_NAME does not)
  BRANCH_NAME=$(echo $GIT_BRANCH | sed -e 's/origin\///g')
fi

DOCKER_IMG_NAME="fpco/ops-fpcomplete-com"
DOC_IMG_BY_SHA="$DOCKER_IMG_NAME:$GIT_REV"
DOC_IMG_BY_BRANCH="$DOCKER_IMG_NAME:$BRANCH_NAME"
ARTIFACTS=$WORKSPACE/artifacts
TARFILE=fpco-ops-platform-docs.tar.gz

# create a docker image with these docs
cat <<EOF > $ARTIFACTS/Dockerfile
FROM yesodweb/warp:latest

# copy files for runtime
ADD $TARFILE /var/www/html/

# this is a runtime image, run hmst/infra-docs
CMD warp --port 3000 --docroot /var/www/html/
EOF

docker pull yesodweb/warp:latest
docker build --tag $DOCKER_IMG_NAME $ARTIFACTS
# only tag and push the new docker image if the build succeeds
if [ $? -eq 0 ]; then
  docker tag -f $DOCKER_IMG_NAME $DOC_IMG_BY_SHA
  docker tag -f $DOCKER_IMG_NAME $DOC_IMG_BY_BRANCH
  docker push $DOC_IMG_BY_SHA
  docker push $DOC_IMG_BY_BRANCH
else
  echo "failed to build docker image with the docs, failing"
  exit 1
fi

# write out that info to our JSON file, to be picked up by CI as an artifact
#DOCKER_INFO="{\"infra-docs\":{\"by_git_sha\":\"$DOC_IMG_BY_SHA\",\"by_branch\": \"$DOC_IMG_BY_BRANCH\"}}"
#echo $DOCKER_INFO > $ARTIFACTS/docker-images.json
#echo "docker image info:"
#cat $ARTIFACTS/docker-images.json

