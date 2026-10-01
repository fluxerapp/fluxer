{{- define "fluxer-infra.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end }}

{{- define "fluxer-infra.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer-infra.labels" -}}
{{ include "fluxer-infra.selectorLabels" . }}
app.kubernetes.io/component: {{ .component }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ include "fluxer-infra.chart" .root }}
{{- end }}

{{- define "fluxer-infra.pick" -}}
{{- $v := get .root.Values .key }}
{{- if hasKey .w .key }}
{{- $v = get .w .key }}
{{- end }}
{{- with $v }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.string" -}}
{{- if and (kindIs "float64" .) (eq . (float64 (int64 .))) }}
{{- int64 . | toString }}
{{- else }}
{{- toString . }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.envList" -}}
{{- $env := deepCopy (.root.Values.env | default dict) }}
{{- range $k, $v := .w.env | default dict }}
{{- if kindIs "invalid" $v }}
{{- $_ := unset $env $k }}
{{- else }}
{{- $_ := set $env $k $v }}
{{- end }}
{{- end }}
{{- range $k, $v := $env }}
{{- if not (kindIs "invalid" $v) }}
- name: {{ $k }}
  value: {{ include "fluxer-infra.string" $v | quote }}
{{- end }}
{{- end }}
{{- with concat (.root.Values.extraEnv | default list) (.w.extraEnv | default list) }}
{{ toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.envFrom" -}}
{{- with concat (.root.Values.envFrom | default list) (.w.envFrom | default list) }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.probes" -}}
{{- $global := .root.Values.probes | default dict }}
{{- $own := .w.probes | default dict }}
{{- range $probe := list "startup" "liveness" "readiness" }}
{{- $p := get $global $probe }}
{{- if hasKey $own $probe }}
{{- $p = get $own $probe }}
{{- end }}
{{- with $p }}
{{ $probe }}Probe:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.topologySpreadConstraints" -}}
{{- $out := list }}
{{- range include "fluxer-infra.pick" (dict "root" .root "w" .w "key" "topologySpreadConstraints") | fromYamlArray }}
{{- $c := deepCopy . }}
{{- if not (hasKey $c "labelSelector") }}
{{- $_ := set $c "labelSelector" (dict "matchLabels" (include "fluxer-infra.selectorLabels" $ | fromYaml)) }}
{{- end }}
{{- $out = append $out $c }}
{{- end }}
{{- with $out }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.replicas" -}}
{{- if kindIs "invalid" .w.replicas }}1{{ else }}{{ .w.replicas }}{{ end }}
{{- end }}

{{- define "fluxer-infra.image" -}}
{{- $ref := printf "%s:%s" .repository .tag }}
{{- with .digest }}
{{- $ref = printf "%s@%s" $ref . }}
{{- end }}
{{- $ref | quote }}
{{- end }}

{{- define "fluxer-infra.podAnnotations" -}}
{{- with merge (deepCopy (.extra | default dict)) (deepCopy (.w.podAnnotations | default dict)) (deepCopy (.root.Values.podAnnotations | default dict)) }}
annotations:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.podSpec" -}}
{{- $root := .root }}
{{- $w := .w }}
{{- with include "fluxer-infra.pick" (dict "root" $root "w" $w "key" "affinity") }}
affinity:
  {{- . | nindent 2 }}
{{- end }}
{{- with include "fluxer-infra.pick" (dict "root" $root "w" $w "key" "imagePullSecrets") }}
imagePullSecrets:
  {{- . | nindent 2 }}
{{- end }}
{{- with include "fluxer-infra.pick" (dict "root" $root "w" $w "key" "nodeSelector") }}
nodeSelector:
  {{- . | nindent 2 }}
{{- end }}
{{- with include "fluxer-infra.pick" (dict "root" $root "w" $w "key" "tolerations") }}
tolerations:
  {{- . | nindent 2 }}
{{- end }}
{{- with include "fluxer-infra.topologySpreadConstraints" . }}
topologySpreadConstraints:
  {{- . | nindent 2 }}
{{- end }}
{{- with include "fluxer-infra.pick" (dict "root" $root "w" $w "key" "podSecurityContext") }}
securityContext:
  {{- . | nindent 2 }}
{{- end }}
{{- if not (kindIs "invalid" $w.terminationGracePeriodSeconds) }}
terminationGracePeriodSeconds: {{ $w.terminationGracePeriodSeconds }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.containerCommon" -}}
{{- $root := .root }}
{{- $w := .w }}
{{- $img := $w.image | default dict }}
image: {{ include "fluxer-infra.image" $img }}
imagePullPolicy: {{ $img.pullPolicy }}
{{- $env := include "fluxer-infra.envList" . | trim }}
{{- if or .env $env }}
env:
{{- with .env }}
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- with $env }}
  {{- . | nindent 2 }}
{{- end }}
{{- end }}
{{- with include "fluxer-infra.envFrom" . }}
envFrom:
  {{- . | nindent 2 }}
{{- end }}
{{- with $w.lifecycle }}
lifecycle:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- include "fluxer-infra.probes" . }}
{{- with $w.resources }}
resources:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- with include "fluxer-infra.pick" (dict "root" $root "w" $w "key" "securityContext") }}
securityContext:
  {{- . | nindent 2 }}
{{- end }}
{{- with concat .mounts ($w.extraVolumeMounts | default list) }}
volumeMounts:
  {{- toYaml . | nindent 2 }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.statefulSetSpec" -}}
{{- $w := .w }}
{{- with include "fluxer-infra.pick" (dict "root" .root "w" $w "key" "updateStrategy") }}
updateStrategy:
  {{- . | nindent 2 }}
{{- end }}
{{- if not (kindIs "invalid" $w.minReadySeconds) }}
minReadySeconds: {{ $w.minReadySeconds }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.volumeClaim" -}}
- metadata:
    name: data
  spec:
    accessModes:
      - ReadWriteOnce
    {{- with .storageClassName }}
    storageClassName: {{ . | quote }}
    {{- end }}
    resources:
      requests:
        storage: {{ .size }}
{{- end }}

{{- define "fluxer-infra.pdb" -}}
{{- with .w.pdb }}
---
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: {{ $.name }}-pdb
  namespace: {{ $.root.Release.Namespace }}
  labels:
    {{- include "fluxer-infra.labels" $ | nindent 4 }}
spec:
  {{- if not (kindIs "invalid" .minAvailable) }}
  minAvailable: {{ .minAvailable }}
  {{- end }}
  {{- if not (kindIs "invalid" .maxUnavailable) }}
  maxUnavailable: {{ .maxUnavailable }}
  {{- end }}
  selector:
    matchLabels:
      {{- include "fluxer-infra.selectorLabels" $ | nindent 6 }}
{{- end }}
{{- end }}

{{- define "fluxer-infra.service" }}
---
apiVersion: v1
kind: Service
metadata:
  name: {{ .svcName }}
  namespace: {{ .root.Release.Namespace }}
  labels:
    {{- include "fluxer-infra.labels" . | nindent 4 }}
spec:
  {{- if .headless }}
  clusterIP: None
  {{- end }}
  {{- if .publishNotReady }}
  publishNotReadyAddresses: true
  {{- end }}
  selector:
    {{- include "fluxer-infra.selectorLabels" . | nindent 4 }}
  ports:
    {{- range .ports }}
    - name: {{ index . 0 }}
      port: {{ index . 1 }}
      targetPort: {{ index . 0 }}
    {{- end }}
{{- end }}

{{- define "fluxer-infra.natsConf" -}}
{{- $w := .Values.nats -}}
{{- with $w.config -}}
listen: 0.0.0.0:4222
http: 0.0.0.0:8222
max_payload: {{ .maxPayload }}
max_pending: {{ .maxPending }}
max_connections: {{ .maxConnections }}
{{- if $w.jetstream.enabled }}
server_name: $POD_NAME

jetstream {
  store_dir: /data
}
{{- end }}

cluster {
  name: {{ .clusterName }}
  listen: 0.0.0.0:6222

  routes = [
{{- range $i := until (int (include "fluxer-infra.replicas" (dict "w" $w))) }}
    nats-route://nats-{{ $i }}.nats-headless.{{ $.Release.Namespace }}.svc.{{ $.Values.clusterDomain }}:6222
{{- end }}
  ]
}
{{ end }}
{{- end }}
