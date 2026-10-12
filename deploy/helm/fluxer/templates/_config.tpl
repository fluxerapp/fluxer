{{- define "fluxer.s3Endpoint" -}}
{{- .Values.s3.endpoint | default (ternary "http://seaweedfs:8333" "" .Values.seaweedfs.enabled) }}
{{- end }}

{{- define "fluxer.configData" -}}
{{- $v := .Values }}
{{- $o := include "fluxer.origin" . }}
{{- $hostOrigin := include "fluxer.hostOrigin" . }}
{{- $media := printf "%s/media" $o }}
{{- $s3 := include "fluxer.s3Endpoint" . }}
{{- $c := dict }}
{{- $_ := set $c "FLUXER_ENV" "production" }}
{{- $_ := set $c "FLUXER_SELF_HOSTED" "true" }}
{{- $_ := set $c "FLUXER_DATABASE_BACKEND" "postgres" }}
{{- $_ := set $c "FLUXER_TRUST_CLIENT_IP_HEADER" "true" }}
{{- $_ := set $c "FLUXER_BASE_DOMAIN" $v.domain }}
{{- $_ := set $c "FLUXER_PUBLIC_SCHEME" $v.scheme }}
{{- $_ := set $c "FLUXER_PUBLIC_PORT" (include "fluxer.string" $v.port) }}
{{- $_ := set $c "FLUXER_PUBLIC_ORIGIN" (trimSuffix "/" ($v.publicOrigin | default "")) }}
{{- $_ := set $c "FLUXER_APP_ENDPOINT" $o }}
{{- $_ := set $c "FLUXER_MARKETING_ENDPOINT" $o }}
{{- $_ := set $c "FLUXER_MEDIA_ENDPOINT" $media }}
{{- $_ := set $c "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT" $media }}
{{- $_ := set $c "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT" $media }}
{{- $_ := set $c "FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT" $media }}
{{- $_ := set $c "FLUXER_ADMIN_ENDPOINT" (printf "%s%s" $o $v.adminBasePath) }}
{{- $_ := set $c "FLUXER_GATEWAY_STATIC_CDN_ENDPOINT" $o }}
{{- $_ := set $c "FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS" $hostOrigin }}
{{- $_ := set $c "FLUXER_PASSKEY_RP_ID" $v.domain }}
{{- $_ := set $c "FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS" $o }}
{{- if $v.postgres.enabled }}
{{- $_ := set $c "FLUXER_POSTGRES_HOST" "postgres" }}
{{- $_ := set $c "FLUXER_POSTGRES_PORT" "5432" }}
{{- $_ := set $c "FLUXER_POSTGRES_DATABASE" $v.postgres.database }}
{{- $_ := set $c "FLUXER_POSTGRES_USERNAME" $v.postgres.username }}
{{- else }}
{{- with $v.postgres.external }}
{{- $_ := set $c "FLUXER_POSTGRES_HOST" (.host | default "") }}
{{- $_ := set $c "FLUXER_POSTGRES_PORT" (include "fluxer.string" (.port | default "")) }}
{{- $_ := set $c "FLUXER_POSTGRES_DATABASE" (.database | default "") }}
{{- $_ := set $c "FLUXER_POSTGRES_USERNAME" (.username | default "") }}
{{- $_ := set $c "FLUXER_POSTGRES_SSL" (include "fluxer.string" (.ssl | default "")) }}
{{- end }}
{{- end }}
{{- if not $v.valkey.enabled }}
{{- $_ := set $c "FLUXER_KV_URL" ($v.valkey.external.url | default "") }}
{{- end }}
{{- $nats := ternary "nats://nats:4222" ($v.nats.external.url | default "") $v.nats.enabled }}
{{- $_ := set $c "FLUXER_NATS_URL" $nats }}
{{- $_ := set $c "FLUXER_NATS_JETSTREAM_URL" $nats }}
{{- $_ := set $c "FLUXER_SVC_NATS_URL" $nats }}
{{- $_ := set $c "FLUXER_SEARCH_ENGINE" "meilisearch" }}
{{- $_ := set $c "FLUXER_SEARCH_URL" (ternary "http://meilisearch:7700" ($v.meilisearch.external.url | default "") $v.meilisearch.enabled) }}
{{- $_ := set $c "FLUXER_S3_ENDPOINT" $s3 }}
{{- $_ := set $c "FLUXER_S3_PUBLIC_ENDPOINT" ($v.s3.publicEndpoint | default $s3) }}
{{- $_ := set $c "FLUXER_S3_REGION" $v.s3.region }}
{{- $_ := set $c "FLUXER_S3_FORCE_PATH_STYLE" (toString $v.s3.forcePathStyle) }}
{{- $_ := set $c "FLUXER_S3_BUCKET_CDN" $v.s3.buckets.cdn }}
{{- $_ := set $c "FLUXER_S3_BUCKET_UPLOADS" $v.s3.buckets.uploads }}
{{- $_ := set $c "FLUXER_S3_BUCKET_REPORTS" $v.s3.buckets.reports }}
{{- $_ := set $c "FLUXER_S3_BUCKET_HARVESTS" $v.s3.buckets.harvests }}
{{- $livekit := include "fluxer.livekitMode" . }}
{{- $_ := set $c "FLUXER_LIVEKIT_ENABLED" (toString (ne $livekit "off")) }}
{{- if eq $livekit "bundled" }}
{{- $_ := set $c "FLUXER_LIVEKIT_INTERNAL_URL" "http://livekit:7880" }}
{{- $_ := set $c "FLUXER_LIVEKIT_DEFAULT_REGION" $v.livekit.defaultRegion }}
{{- $_ := set $c "FLUXER_LIVEKIT_URL" (printf "%s/livekit" $hostOrigin) }}
{{- else if eq $livekit "external" }}
{{- $_ := set $c "FLUXER_LIVEKIT_INTERNAL_URL" $v.livekit.external.url }}
{{- $_ := set $c "FLUXER_LIVEKIT_DEFAULT_REGION" $v.livekit.defaultRegion }}
{{- $_ := set $c "FLUXER_LIVEKIT_URL" $v.livekit.external.publicUrl }}
{{- end }}
{{- $_ := set $c "FLUXER_API_PRESIGNED_HARVEST_DOWNLOADS_ENABLED" "false" }}
{{- $_ := set $c "FLUXER_EMAIL_FROM_EMAIL" "noreply@localhost" }}
{{- $_ := set $c "FLUXER_VAPID_EMAIL" ($v.email | default (printf "admin@%s" $v.domain)) }}
{{- $_ := set $c "FLUXER_INTERNAL_API_ENDPOINT" "http://api:8080" }}
{{- $_ := set $c "FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT" "http://media-proxy:8080" }}
{{- range $k, $val := $v.env | default dict }}
{{- $_ := set $c $k (include "fluxer.string" ($val | default "")) }}
{{- end }}
{{- $out := dict }}
{{- range $k, $val := $c }}
{{- if $val }}
{{- $_ := set $out $k $val }}
{{- end }}
{{- end }}
{{- toYaml $out }}
{{- end }}

{{- define "fluxer.appProxyConfigData" -}}
{{- $v := .Values }}
{{- $s3 := include "fluxer.s3Endpoint" . }}
{{- $c := dict }}
{{- $_ := set $c "FLUXER_APP_PROXY_PORT" "8080" }}
{{- $_ := set $c "DISCOVERY_UPSTREAM_URL" "http://api:8080/.well-known/fluxer" }}
{{- $_ := set $c "FLUXER_S3_PUBLIC_ENDPOINT" ($v.s3.publicEndpoint | default $s3) }}
{{- $_ := set $c "FLUXER_S3_BUCKET_UPLOADS" $v.s3.buckets.uploads }}
{{- $_ := set $c "FLUXER_MEDIA_ENDPOINT" (printf "%s/media" (include "fluxer.origin" .)) }}
{{- range $k, $val := $v.appProxy.config | default dict }}
{{- $_ := set $c $k (include "fluxer.string" ($val | default "")) }}
{{- end }}
{{- $out := dict }}
{{- range $k, $val := $c }}
{{- if $val }}
{{- $_ := set $out $k $val }}
{{- end }}
{{- end }}
{{- toYaml $out }}
{{- end }}
