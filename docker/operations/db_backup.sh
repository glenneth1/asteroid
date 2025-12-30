#!/usr/bin/env bash

## Backup the postgresql instance running in our local
## asteroid-postgres docker container

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Source the runtime environment for asteroid.radio. creds, paths &c

# if [[ -f "${HOME}"/SourceCode/lisp/asteroid/docker/environment.sh ]]; then
#     source "${HOME}"/SourceCode/lisp/asteroid/docker/environment.sh
# fi

BACKUP_DIR=${SCRIPT_DIR}/db_backups
mkdir -p "$BACKUP_DIR"

docker exec asteroid-postgres sh -c 'pg_dump $POSTGRES_DB -U $POSTGRES_USER -F p' | xz --compress -c > "${BACKUP_DIR}"/db_"$(date +%Y-%m-%d-%H.%M.%S)"_backup.sql.xz

