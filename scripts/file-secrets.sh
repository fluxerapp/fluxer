#!/usr/bin/env bash

# usage: file_env VAR [DEFAULT]
#    ie: file_env 'XYZ_DB_PASSWORD' 'example'
# (will allow for "$XYZ_DB_PASSWORD_FILE" to fill in the value of
#  "$XYZ_DB_PASSWORD" from a file, especially for Docker's secrets feature)
file_env() {
	local var="$1"
	local fileVar="${var}_FILE"
	local def="${2:-}"
	if [ "${!var:-}" ] && [ "${!fileVar:-}" ]; then
		echo >&2 "error: both $var and $fileVar are set (but are exclusive)"
		exit 1
	fi
	local val="$def"
	if [ "${!var:-}" ]; then
		val="${!var}"
	elif [ "${!fileVar:-}" ]; then
		val="$(< "${!fileVar}")"
	fi
	export "$var"="$val"
	unset "$fileVar"
}

file_env 'FLUXER_POSTGRES_PASSWORD' 
file_env 'LIVEKIT_API_SECRET' 
file_env 'FLUXER_SEARCH_API_KEY'
file_env 'FLUXER_S3_SECRET_ACCESS_KEY' 
file_env 'AWS_SECRET_ACCESS_KEY' 
file_env 'FLUXER_LIVEKIT_API_SECRET'
file_env 'FLUXER_SUDO_MODE_SECRET' 
file_env 'FLUXER_CONNECTION_INITIATION_SECRET' 
file_env 'FLUXER_VAPID_PUBLIC_KEY'
file_env 'FLUXER_VAPID_PRIVATE_KEY' 
file_env 'FLUXER_GATEWAY_RPC_AUTH_TOKEN' 
file_env 'FLUXER_MEDIA_PROXY_SECRET_KEY'
file_env 'FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64'
file_env 'FLUXER_ADMIN_SECRET_KEY_BASE'
file_env 'FLUXER_ADMIN_OAUTH_CLIENT_SECRET'  
file_env 'MEILI_MASTER_KEY'


exec "$@"
# pulled from https://github.com/BretFisher/ama/discussions/215#:~:text=for%20the%20registry.-,entrypoint.sh,-%23!/usr/bin
