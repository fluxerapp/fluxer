{{- define "fluxer.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{- define "fluxer.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 52 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 52 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 52 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{- define "fluxer.selectorLabels" -}}
app.kubernetes.io/name: {{ include "fluxer.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{- define "fluxer.labels" -}}
helm.sh/chart: {{ printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{ include "fluxer.selectorLabels" . }}
app.kubernetes.io/version: {{ include "fluxer.tag" . | trunc 63 | trimSuffix "-" | quote }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{- define "fluxer.tag" -}}
{{- .Values.image.tag | default .Chart.AppVersion }}
{{- end }}

{{- define "fluxer.image" -}}
{{- if .Values.image.digest }}
{{- printf "%s@%s" .Values.image.repository .Values.image.digest }}
{{- else }}
{{- printf "%s:%s" .Values.image.repository (include "fluxer.tag" .) }}
{{- end }}
{{- end }}

{{- define "fluxer.upgradeCommand" -}}
{{- printf "helm upgrade %s oci://ghcr.io/fluxerapp/charts/fluxer -n %s --version {version} --reset-then-reuse-values" .Release.Name .Release.Namespace }}
{{- if .Values.image.tag }} --set image.tag={version}{{ end }}
{{- if .Values.image.digest }} --set image.digest={{ end }}
{{- end }}

{{- define "fluxer.voicePorts" -}}
{{- if eq .Values.voice.expose "nodePort" }}
{{- dict "tcp" (int .Values.voice.nodePorts.tcp) "udp" (int .Values.voice.nodePorts.udp) | toJson }}
{{- else }}
{{- dict "tcp" 7881 "udp" 7882 | toJson }}
{{- end }}
{{- end }}

{{- define "fluxer.config" -}}
{{- $v := .Values }}
{{- $owned := dict
  "FLUXER_DOMAIN" "domain"
  "FLUXER_EDGE_MODE" "edge.mode"
  "FLUXER_EDGE_BIND" "edge.mode"
  "FLUXER_EDGE_PROTOCOLS" "edge.loadBalancer.http3"
  "FLUXER_EDGE_TRUSTED_PROXIES" "edge.trustedProxies"
  "FLUXER_HTTP_PORT" "edge.mode"
  "FLUXER_HTTPS_PORT" "edge.mode"
  "FLUXER_LIVEKIT_ENABLED" "voice.mode"
  "FLUXER_LIVEKIT_BUNDLED" "voice.mode"
  "FLUXER_LIVEKIT_NODE_IP" "voice.publicIP"
  "FLUXER_LIVEKIT_TCP_PORT" "voice.nodePorts"
  "FLUXER_LIVEKIT_UDP_PORT" "voice.nodePorts"
  "FLUXER_BACKUP_SCHEDULE" "backup.schedule"
  "FLUXER_BACKUP_KEEP" "backup.keep"
  "FLUXER_BACKUP_HOUR" "backup.hour"
  "FLUXER_POSTGRES_URL" "postgres.externalSecret"
  "FLUXER_DATA_DIR" "persistence"
  "FLUXER_CONTAINER" "image"
  "FLUXER_INSTALL_METHOD" "config.FLUXER_UPGRADE_COMMAND" }}
{{- range $k, $_ := $v.config }}
{{- with get $owned $k }}
{{- fail (printf "config.%s is set by the chart. Use %s instead." $k .) }}
{{- end }}
{{- if and (regexMatch "(PASSWORD|SECRET|TOKEN|COOKIE|_KEYS?|_BASE64|_KEY_BASE|TRUSTED_CALLERS)$" $k) (not (hasSuffix "PUBLIC_KEY" $k)) }}
{{- fail (printf "config.%s looks like a password or key, and config is stored in plain text. Put it in the Secret set in existingSecret." $k) }}
{{- end }}
{{- end }}
{{- $c := dict "FLUXER_DOMAIN" $v.domain "FLUXER_LIVEKIT_ENABLED" (toString (ne $v.voice.mode "off")) }}
{{- $_ := set $c "FLUXER_INSTALL_METHOD" "helm" }}
{{- $_ := set $c "FLUXER_UPGRADE_COMMAND" (include "fluxer.upgradeCommand" .) }}
{{- if eq $v.voice.mode "external" }}
{{- $_ := set $c "FLUXER_LIVEKIT_BUNDLED" "false" }}
{{- end }}
{{- if eq (include "fluxer.edgeMode" .) "proxy" }}
{{- $_ := set $c "FLUXER_EDGE_MODE" "proxy" }}
{{- $_ := set $c "FLUXER_EDGE_TRUSTED_PROXIES" $v.edge.trustedProxies }}
{{- else if not $v.edge.loadBalancer.http3 }}
{{- $_ := set $c "FLUXER_EDGE_PROTOCOLS" "h1 h2" }}
{{- end }}
{{- if eq $v.voice.mode "bundled" }}
{{- $nodeIP := $v.voice.publicIP | default (and (eq $v.voice.expose "loadBalancer") $v.voice.service.loadBalancerIP) }}
{{- with $nodeIP }}
{{- $_ := set $c "FLUXER_LIVEKIT_NODE_IP" . }}
{{- end }}
{{- $onNode := and (eq (include "fluxer.edgeMode" .) "bundled") (eq $v.edge.expose "hostPort") (eq $v.voice.expose "hostPort") }}
{{- if and (not $nodeIP) (include "fluxer.ipDomain" .) (not $onNode) }}
{{- $_ := set $c "FLUXER_LIVEKIT_USE_EXTERNAL_IP" "true" }}
{{- end }}
{{- if eq $v.voice.expose "nodePort" }}
{{- $_ := set $c "FLUXER_LIVEKIT_TCP_PORT" (toString $v.voice.nodePorts.tcp) }}
{{- $_ := set $c "FLUXER_LIVEKIT_UDP_PORT" (toString $v.voice.nodePorts.udp) }}
{{- end }}
{{- end }}
{{- with $v.backup.schedule }}
{{- $_ := set $c "FLUXER_BACKUP_SCHEDULE" . }}
{{- $_ := set $c "FLUXER_BACKUP_KEEP" (toString $v.backup.keep) }}
{{- $_ := set $c "FLUXER_BACKUP_HOUR" (toString $v.backup.hour) }}
{{- end }}
{{- range $k, $val := $v.config }}
{{- if and (kindIs "float64" $val) (eq (floor $val) $val) }}
{{- $_ := set $c $k ($val | int64 | toString) }}
{{- else }}
{{- $_ := set $c $k (toString $val) }}
{{- end }}
{{- end }}
{{- toYaml $c }}
{{- end }}

{{- define "fluxer.edgeMode" -}}
{{- if .Values.edge.mode }}{{ .Values.edge.mode }}{{ else if and (include "fluxer.ipDomain" .) (or .Values.ingress.enabled .Values.httpRoute.enabled) }}bundled{{ else }}proxy{{ end }}
{{- end }}

{{- define "fluxer.bareIP" -}}
{{- if include "fluxer.ipDomain" . }}{{ trimSuffix "]" (trimPrefix "[" .Values.domain) }}{{ end }}
{{- end }}

{{- define "fluxer.ipDomain" -}}
{{- if or (regexMatch "^[0-9]+(\\.[0-9]+){3}$" .Values.domain) (contains ":" .Values.domain) }}true{{ end }}
{{- end }}

{{- define "fluxer.validIP" -}}
{{- $octet := "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])" }}
{{- $v4 := printf "^%s(\\.%s){3}$" $octet $octet }}
{{- $d := trimSuffix "]" (trimPrefix "[" .Values.domain) | lower }}
{{- if not (contains ":" $d) }}
{{- if regexMatch $v4 $d }}true{{ end }}
{{- else }}
{{- $halves := splitList "::" $d }}
{{- $count := 0 }}
{{- $ok := le (len $halves) 2 }}
{{- range $i, $half := $halves }}
{{- if $half }}
{{- $groups := splitList ":" $half }}
{{- range $j, $g := $groups }}
{{- if and (eq $i (sub (len $halves) 1)) (eq $j (sub (len $groups) 1)) (regexMatch $v4 $g) }}
{{- $count = add $count 2 }}
{{- else if regexMatch "^[0-9a-f]{1,4}$" $g }}
{{- $count = add $count 1 }}
{{- else }}
{{- $ok = false }}
{{- end }}
{{- end }}
{{- end }}
{{- end }}
{{- if and $ok (or (and (eq (len $halves) 2) (le $count 7)) (eq $count 8)) }}true{{ end }}
{{- end }}
{{- end }}

{{- define "fluxer.host" -}}
{{- $d := .Values.domain | lower }}
{{- if and (contains ":" $d) (not (hasPrefix "[" $d)) }}[{{ $d }}]{{ else }}{{ $d }}{{ end }}
{{- end }}

{{- define "fluxer.validate" -}}
{{- if not .Values.domain }}
{{- fail "Set domain to the address people open, without https://." }}
{{- end }}
{{- if regexMatch "(^|\\.)example\\.(com|net|org)$" .Values.domain }}
{{- fail (printf "domain is %s, an example name. Set it to your own domain." .Values.domain) }}
{{- end }}
{{- if and (include "fluxer.ipDomain" .) (not (include "fluxer.validIP" .)) }}
{{- fail (printf "domain is %s, which is not a valid IPv4 or IPv6 address. Write an IPv4 address as four numbers from 0 to 255, such as 192.0.2.10, and an IPv6 address in hex groups with at most one ::, such as 2001:db8::10." .Values.domain) }}
{{- end }}
{{- $bare := include "fluxer.bareIP" . | lower }}
{{- if or (has $bare (list "0.0.0.0" "::" "255.255.255.255")) (regexMatch "^(22[4-9]|23[0-9])\\." $bare) (hasPrefix "ff" $bare) }}
{{- fail (printf "domain is %s, which no device can open. Use the address of this machine or its load balancer." .Values.domain) }}
{{- end }}
{{- if and (include "fluxer.ipDomain" .) (eq (include "fluxer.edgeMode" .) "proxy") (or .Values.ingress.enabled .Values.httpRoute.enabled) }}
{{- fail (printf "domain is the IP address %s, and an Ingress or HTTPRoute only routes domain names. Set edge.mode to bundled, or turn off ingress.enabled and httpRoute.enabled and send traffic to port 8080 of the Service yourself." .Values.domain) }}
{{- end }}
{{- if and (eq .Values.edge.expose "hostPort") (eq (include "fluxer.edgeMode" .) "proxy") }}
{{- fail "edge.expose is hostPort, which only opens ports 80 and 443 in bundled mode. Set edge.mode to bundled, or leave edge.expose at loadBalancer." }}
{{- end }}
{{- $old := lookup "apps/v1" "Deployment" .Release.Namespace "edge" }}
{{- if and $old (eq (dig "metadata" "labels" "app.kubernetes.io/instance" "" $old) .Release.Name) (eq (dig "metadata" "labels" "app.kubernetes.io/part-of" "" $old) "fluxer") }}
{{- fail "This release runs the 0.x chart, and upgrading it would start an empty instance. Install this chart under a new release name and move the data with fluxerd import. See https://docs.fluxer.com/operator/moving/#moving-from-the-0x-chart" }}
{{- end }}
{{- $legacy := list }}
{{- range $k := list "admin" "adminBasePath" "api" "appProxy" "clusterChecks" "clusterDomain" "email" "env" "gateway" "initImage" "livekit" "mediaProxy" "meilisearch" "nats" "port" "publicOrigin" "push" "s3" "scheme" "seaweedfs" "secrets" "staticProxy" "svc" "valkey" "worker" }}
{{- if hasKey $.Values $k }}
{{- $legacy = append $legacy $k }}
{{- end }}
{{- end }}
{{- if $legacy }}
{{- fail (printf "%s are values of the 0.x chart, so this release or values file is from the 0.x chart. Install this chart under a new release name and move the data with fluxerd import. See https://docs.fluxer.com/operator/moving/#moving-from-the-0x-chart" (join ", " $legacy)) }}
{{- end }}
{{- end }}

{{- define "fluxer.annotations" -}}
{{- $out := dict }}
{{- range $k, $v := . }}
{{- if and (kindIs "float64" $v) (eq (floor $v) $v) }}
{{- $_ := set $out $k ($v | int64 | toString) }}
{{- else }}
{{- $_ := set $out $k (toString $v) }}
{{- end }}
{{- end }}
{{- toYaml $out }}
{{- end }}
