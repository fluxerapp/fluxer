{{- define "fluxer.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end }}

{{- define "fluxer.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer.podLabels" -}}
{{ include "fluxer.selectorLabels" . }}
{{- with .component }}
app.kubernetes.io/component: {{ . }}
{{- end }}
app.kubernetes.io/part-of: fluxer
{{- end }}

{{- define "fluxer.labels" -}}
{{ include "fluxer.podLabels" . }}
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
app.kubernetes.io/version: {{ .root.Chart.AppVersion | quote }}
helm.sh/chart: {{ include "fluxer.chart" .root }}
{{- end }}

{{- define "fluxer.string" -}}
{{- if and (kindIs "float64" .) (eq . (float64 (int64 .))) }}
{{- int64 . | toString }}
{{- else }}
{{- toString . }}
{{- end }}
{{- end }}

{{- define "fluxer.origin" -}}
{{- $v := .Values }}
{{- if $v.publicOrigin }}
{{- trimSuffix "/" $v.publicOrigin }}
{{- else }}
{{- $default := ternary 443 80 (eq $v.scheme "https") }}
{{- if eq (int $v.port) $default }}
{{- printf "%s://%s" $v.scheme $v.domain }}
{{- else }}
{{- printf "%s://%s:%d" $v.scheme $v.domain (int $v.port) }}
{{- end }}
{{- end }}
{{- end }}

{{- define "fluxer.hostOrigin" -}}
{{- $v := .Values }}
{{- if $v.publicOrigin }}
{{- trimSuffix "/" $v.publicOrigin }}
{{- else }}
{{- printf "%s://%s:%d" $v.scheme $v.domain (int $v.port) }}
{{- end }}
{{- end }}

{{- define "fluxer.secretName" -}}
{{- .Values.secrets.existingSecret | default "fluxer-env" }}
{{- end }}

{{- define "fluxer.thirdPartyImage" -}}
{{- $ref := printf "%s:%s" .repository (include "fluxer.string" .tag) }}
{{- with .digest }}
{{- $ref = printf "%s@%s" $ref . }}
{{- end }}
{{- $ref | quote }}
{{- end }}

{{- define "fluxer.image" -}}
{{- $v := .root.Values.image }}
{{- $img := (.w | default dict).image | default dict }}
{{- $registry := $img.registry | default $v.registry }}
{{- $tag := $img.tag | default (get ($v.tags | default dict) .image) | default $v.tag }}
{{- $ref := printf "%s/%s:%s" $registry .image (include "fluxer.string" $tag) }}
{{- with $img.digest | default (get ($v.digests | default dict) .image) }}
{{- $ref = printf "%s@%s" $ref . }}
{{- end }}
{{- $ref | quote }}
{{- end }}

{{- define "fluxer.pullPolicy" -}}
{{- (((.w | default dict).image | default dict).pullPolicy) | default .root.Values.image.pullPolicy }}
{{- end }}

{{- define "fluxer.pullSecrets" -}}
{{- with .Values.image.pullSecrets }}
imagePullSecrets:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- end }}

{{- define "fluxer.podSecurityContext" -}}
runAsNonRoot: true
{{- with .uid }}
runAsUser: {{ . }}
runAsGroup: {{ $.gid | default . }}
fsGroup: {{ $.gid | default . }}
fsGroupChangePolicy: OnRootMismatch
{{- end }}
seccompProfile:
  type: RuntimeDefault
{{- end }}

{{- define "fluxer.containerSecurityContext" -}}
allowPrivilegeEscalation: false
capabilities:
  drop:
    - ALL
{{- end }}

{{- define "fluxer.scheduling" -}}
{{- $w := .w | default dict }}
{{- with $w.nodeSelector }}
nodeSelector:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- with $w.tolerations }}
tolerations:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- with $w.affinity }}
affinity:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- $spread := list }}
{{- range $w.topologySpreadConstraints }}
{{- $c := deepCopy . }}
{{- if not (hasKey $c "labelSelector") }}
{{- $_ := set $c "labelSelector" (dict "matchLabels" (include "fluxer.selectorLabels" $ | fromYaml)) }}
{{- end }}
{{- $spread = append $spread $c }}
{{- end }}
{{- with $spread }}
topologySpreadConstraints:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- end }}

{{- define "fluxer.pvc" -}}
- metadata:
    name: data
  spec:
    accessModes:
      - ReadWriteOnce
    {{- with .storageClassName }}
    storageClassName: {{ . }}
    {{- end }}
    resources:
      requests:
        storage: {{ .size }}
{{- end }}

{{- define "fluxer.vapidPair" -}}
{{- $der := genPrivateKey "ecdsa" | replace "-----BEGIN EC PRIVATE KEY-----" "" | replace "-----END EC PRIVATE KEY-----" "" | nospace | b64dec -}}
private: {{ substr 7 39 $der | b64enc | replace "+" "-" | replace "/" "_" | trimAll "=" }}
public: {{ substr 56 121 $der | b64enc | replace "+" "-" | replace "/" "_" | trimAll "=" }}
{{- end }}

{{- define "fluxer.livekitMode" -}}
{{- if .Values.livekit.enabled }}bundled{{ else if .Values.livekit.external.url }}external{{ else }}off{{ end }}
{{- end }}

{{- define "fluxer.noGenerateKeys" -}}
{{- $keys := list }}
{{- $extra := .Values.secrets.extra | default dict }}
{{- if and (not .Values.postgres.enabled) (not (get $extra "FLUXER_POSTGRES_URL")) }}
{{- $keys = append $keys "FLUXER_POSTGRES_PASSWORD" }}
{{- end }}
{{- if not .Values.meilisearch.enabled }}
{{- $keys = append $keys "FLUXER_SEARCH_API_KEY" }}
{{- end }}
{{- if not .Values.seaweedfs.enabled }}
{{- $keys = append $keys "FLUXER_S3_SECRET_ACCESS_KEY" }}
{{- end }}
{{- if eq (include "fluxer.livekitMode" .) "external" }}
{{- $keys = append $keys "FLUXER_LIVEKIT_API_SECRET" }}
{{- end }}
{{- toJson $keys }}
{{- end }}

{{- define "fluxer.requiredSecretKeys" -}}
{{- $keys := list }}
{{- $extra := .Values.secrets.extra | default dict }}
{{- range $k, $_ := .Values.secrets.values }}
{{- if and (eq $k "FLUXER_POSTGRES_PASSWORD") (get $extra "FLUXER_POSTGRES_URL") }}
{{- else if and (hasPrefix "FLUXER_LIVEKIT_" $k) (eq (include "fluxer.livekitMode" $) "off") }}
{{- else if and (eq $k "FLUXER_VALKEY_PASSWORD") (not $.Values.valkey.enabled) }}
{{- else if and (eq $k "FLUXER_NATS_AUTH_TOKEN") (not $.Values.nats.enabled) }}
{{- else if and (eq $k "FLUXER_SETUP_TOKEN") (or $.Values.secrets.existingSecret (and (not $.Values.secrets.generate) (not (include "fluxer.string" (get $.Values.secrets.values $k | default ""))))) }}
{{- else }}
{{- $keys = append $keys $k }}
{{- end }}
{{- end }}
{{- toJson $keys }}
{{- end }}

{{- define "fluxer.resolveSecrets" -}}
{{- $v := .Values }}
{{- $existing := dict }}
{{- $found := lookup "v1" "Secret" .Release.Namespace (include "fluxer.secretName" .) }}
{{- range $k, $val := ($found.data | default dict) }}
{{- $_ := set $existing $k (b64dec $val) }}
{{- end }}
{{- $out := dict }}
{{- if $v.secrets.existingSecret }}
{{- $out = $existing }}
{{- else }}
{{- $noGenerate := include "fluxer.noGenerateKeys" . | fromJsonArray }}
{{- $explicit := $v.secrets.values | default dict }}
{{- $vapidPublic := include "fluxer.string" (get $explicit "FLUXER_VAPID_PUBLIC_KEY" | default "") }}
{{- $vapidPrivate := include "fluxer.string" (get $explicit "FLUXER_VAPID_PRIVATE_KEY" | default "") }}
{{- if and $vapidPublic $vapidPrivate }}
{{- else if or $vapidPublic $vapidPrivate }}
{{- fail "secrets.values.FLUXER_VAPID_PUBLIC_KEY and secrets.values.FLUXER_VAPID_PRIVATE_KEY must be set together, or both left empty to generate a pair" }}
{{- else if and (get $existing "FLUXER_VAPID_PUBLIC_KEY") (get $existing "FLUXER_VAPID_PRIVATE_KEY") }}
{{- $vapidPublic = get $existing "FLUXER_VAPID_PUBLIC_KEY" }}
{{- $vapidPrivate = get $existing "FLUXER_VAPID_PRIVATE_KEY" }}
{{- else if $v.secrets.generate }}
{{- $pair := include "fluxer.vapidPair" . | fromYaml }}
{{- $vapidPublic = $pair.public }}
{{- $vapidPrivate = $pair.private }}
{{- else }}
{{- fail "secrets.values.FLUXER_VAPID_PUBLIC_KEY and secrets.values.FLUXER_VAPID_PRIVATE_KEY are empty and secrets.generate is false" }}
{{- end }}
{{- $_ := set $out "FLUXER_VAPID_PUBLIC_KEY" $vapidPublic }}
{{- $_ := set $out "FLUXER_VAPID_PRIVATE_KEY" $vapidPrivate }}
{{- $required := include "fluxer.requiredSecretKeys" . | fromJsonArray }}
{{- $outsideToken := include "fluxer.string" (get $explicit "FLUXER_NATS_AUTH_TOKEN" | default "") }}
{{- if and $outsideToken (not $v.nats.enabled) }}
{{- $_ := set $out "FLUXER_NATS_AUTH_TOKEN" $outsideToken }}
{{- end }}
{{- range $k := $required }}
{{- if not (hasPrefix "FLUXER_VAPID_" $k) }}
{{- $given := include "fluxer.string" (get $explicit $k | default "") }}
{{- if $given }}
{{- $_ := set $out $k $given }}
{{- else if has $k $noGenerate }}
{{- fail (printf "secrets.values.%s is required when the matching service is not bundled" $k) }}
{{- else if get $existing $k }}
{{- $_ := set $out $k (get $existing $k) }}
{{- else if $v.secrets.generate }}
{{- if eq $k "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64" }}
{{- $_ := set $out $k (randBytes 32) }}
{{- else }}
{{- $_ := set $out $k (randAlphaNum 64) }}
{{- end }}
{{- else }}
{{- fail (printf "secrets.values.%s is empty and secrets.generate is false" $k) }}
{{- end }}
{{- end }}
{{- end }}
{{- range $k, $val := $v.secrets.extra | default dict }}
{{- $s := include "fluxer.string" ($val | default "") }}
{{- if $s }}
{{- $_ := set $out $k $s }}
{{- end }}
{{- end }}
{{- end }}
{{- toJson $out }}
{{- end }}

{{- define "fluxer.secretData" -}}
{{- if not (hasKey .Values "fluxerResolvedSecrets") }}
{{- $_ := set .Values "fluxerResolvedSecrets" (include "fluxer.resolveSecrets" . | fromJson) }}
{{- end }}
{{- toJson (get .Values "fluxerResolvedSecrets") }}
{{- end }}

{{- define "fluxer.secretEnv" -}}
- name: {{ .name }}
  valueFrom:
    secretKeyRef:
      name: {{ include "fluxer.secretName" .root }}
      key: {{ .key }}
{{- end }}

{{- define "fluxer.instanceChecksum" -}}
{{- $parts := list (include "fluxer.configData" .) (include "fluxer.appProxyConfigData" .) (toJson .Values.secrets.values) (toJson (.Values.secrets.extra | default dict)) (include "fluxer.string" .Values.image.revision) (include "fluxer.secretData" .) }}
{{- join "\n" $parts | sha256sum }}
{{- end }}

{{- define "fluxer.envList" -}}
{{- $w := .w | default dict }}
{{- $own := $w.env | default dict }}
{{- $base := .env | default list }}
{{- $names := list }}
{{- range $base }}
{{- $names = append $names .name }}
{{- end }}
{{- $out := list }}
{{- range $base }}
{{- if hasKey $own .name }}
{{- $val := get $own .name }}
{{- if not (kindIs "invalid" $val) }}
{{- $out = append $out (dict "name" .name "value" (include "fluxer.string" $val)) }}
{{- end }}
{{- else }}
{{- $out = append $out . }}
{{- end }}
{{- end }}
{{- range $k := keys $own | sortAlpha }}
{{- $val := get $own $k }}
{{- if and (not (has $k $names)) (not (kindIs "invalid" $val)) }}
{{- $out = append $out (dict "name" $k "value" (include "fluxer.string" $val)) }}
{{- end }}
{{- end }}
{{- $out = concat $out ($w.extraEnv | default list) }}
{{- with $out }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer.probes" -}}
{{- $own := (.w | default dict).probes | default dict }}
{{- range $probe := list "startup" "liveness" "readiness" }}
{{- $p := get ($.probes | default dict) $probe }}
{{- if hasKey $own $probe }}
{{- $p = get $own $probe }}
{{- end }}
{{- with $p }}
{{ $probe }}Probe:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- end }}
{{- end }}

{{- define "fluxer.waitFor" -}}
{{- with .waitFor }}
initContainers:
  - name: wait-for-dependencies
    image: {{ include "fluxer.thirdPartyImage" $.root.Values.initImage }}
    imagePullPolicy: {{ $.root.Values.initImage.pullPolicy }}
    command:
      - sh
      - -c
      - {{ printf "for t in %s; do until nc -z -w 2 ${t%%:*} ${t##*:}; do echo waiting for $t; sleep 2; done; done" (join " " .) | quote }}
    resources:
      requests:
        cpu: 10m
        memory: 16Mi
      limits:
        memory: 32Mi
    securityContext:
      runAsUser: 65532
      runAsGroup: 65532
      {{- include "fluxer.containerSecurityContext" . | nindent 6 }}
{{- end }}
{{- end }}

{{- define "fluxer.appWaitFor" -}}
{{- $out := list }}
{{- if and .usesNats .root.Values.nats.enabled }}
{{- $out = append $out "nats:4222" }}
{{- end }}
{{- if and .usesPostgres .root.Values.postgres.enabled }}
{{- $out = append $out "postgres:5432" }}
{{- end }}
{{- if and .usesS3 .root.Values.seaweedfs.enabled }}
{{- $out = append $out "seaweedfs:8333" }}
{{- end }}
{{- if and .usesKv .root.Values.valkey.enabled }}
{{- $out = append $out "valkey:6379" }}
{{- end }}
{{- if .usesSvc }}
{{- range $name := list "gifs" "messages" "snowflakes" "unfurl" "users" }}
{{- $svc := get $.root.Values.svc $name | default dict }}
{{- if gt (int (include "fluxer.replicas" (dict "w" ($svc.router | default dict)))) 0 }}
{{- $out = append $out (printf "%s:8090" $name) }}
{{- end }}
{{- if gt (int (include "fluxer.replicas" (dict "w" ($svc.shard | default dict)))) 0 }}
{{- $out = append $out (printf "%s-shard:8090" $name) }}
{{- end }}
{{- end }}
{{- end }}
{{- toJson $out }}
{{- end }}
