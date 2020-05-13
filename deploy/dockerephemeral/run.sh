#!/usr/bin/env bash TODO: update quay

# run.sh should work no matter what is the current directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"

# If there are schema changes and you don't force pull the docker
# migrations, you may run out of sync and you would get this error
# message (or a similar one):
#
#     brig: Schema Version too old! Expecting at least: 49, but got: 48
#
# So we always pull these migration images first.
docker pull quay.io/society/brig-schema
docker pull quay.io/society/galley-schema
docker pull quay.io/society/gundeck-schema
docker pull quay.io/society/spar-schema
docker pull quay.io/society/brig-index

docker-compose --file "$DOCKER_FILE" up
