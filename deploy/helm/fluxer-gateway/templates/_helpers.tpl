{{- define "gateway.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "gateway.labels" -}}
{{ include "gateway.selectorLabels" . }}
{{- with .component }}
app.kubernetes.io/component: {{ . }}
{{- end }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ printf "%s-%s" .root.Chart.Name .root.Chart.Version | replace "+" "_" }}
{{- end }}

{{- define "gateway.headlessName" -}}
{{ printf "%s-headless" .Release.Name }}
{{- end }}

{{- define "gateway.pick" -}}
{{- $v := get .root.Values .key }}
{{- if hasKey .w .key }}
{{- $v = get .w .key }}
{{- end }}
{{- with $v }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "gateway.string" -}}
{{- if and (kindIs "float64" .) (eq . (float64 (int64 .))) }}
{{- int64 . | toString }}
{{- else }}
{{- toString . }}
{{- end }}
{{- end }}

{{- define "gateway.envList" -}}
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
  value: {{ include "gateway.string" $v | quote }}
{{- end }}
{{- end }}
{{- with concat (.root.Values.extraEnv | default list) (.w.extraEnv | default list) }}
{{ toYaml . }}
{{- end }}
{{- end }}

{{- define "gateway.envFrom" -}}
{{- with concat (.root.Values.envFrom | default list) (.w.envFrom | default list) }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "gateway.podAnnotations" -}}
{{- with merge (deepCopy (.w.podAnnotations | default dict)) (deepCopy (.root.Values.podAnnotations | default dict)) }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "gateway.probes" -}}
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

{{- define "gateway.topologySpreadConstraints" -}}
{{- $out := list }}
{{- range include "gateway.pick" (dict "root" .root "w" .w "key" "topologySpreadConstraints") | fromYamlArray }}
{{- $c := deepCopy . }}
{{- if not (hasKey $c "labelSelector") }}
{{- $_ := set $c "labelSelector" (dict "matchLabels" (include "gateway.selectorLabels" $ | fromYaml)) }}
{{- end }}
{{- $out = append $out $c }}
{{- end }}
{{- with $out }}
{{- toYaml . }}
{{- end }}
{{- end }}

{{- define "gateway.image" -}}
{{- $img := .w.image | default dict }}
{{- $v := .root.Values.image }}
{{- $repo := $img.repository | default (printf "%s/%s" $v.registry ($img.name | default "fluxer-gateway")) }}
{{- $ref := printf "%s:%s" $repo ($img.tag | default $v.tag) }}
{{- with $img.digest }}
{{- $ref = printf "%s@%s" $ref . }}
{{- end }}
{{- $ref | quote }}
{{- end }}

{{- define "gateway.replicas" -}}
{{- if kindIs "invalid" .w.replicas }}1{{ else }}{{ .w.replicas }}{{ end }}
{{- end }}

{{- define "gateway.env" -}}
{{- $root := .root }}
{{- $w := .w -}}
{{- with $w.role }}
- name: FLUXER_GATEWAY_ROLE
  value: {{ . | quote }}
{{- end }}
{{- if not (kindIs "invalid" $w.buildVersion) }}
- name: BUILD_VERSION
  value: {{ include "gateway.string" $w.buildVersion | quote }}
{{- end }}
- name: POD_IP
  valueFrom:
    fieldRef:
      apiVersion: v1
      fieldPath: status.podIP
- name: FLUXER_ERLANG_NODE_NAME
  value: fluxer_gateway@$(POD_IP)
- name: FLUXER_ERLANG_DIST_PORT
  value: "8081"
- name: FLUXER_GATEWAY_CLUSTER_ENABLED
  value: "true"
- name: FLUXER_GATEWAY_CLUSTER_DISCOVERY_DNS_NAME
  value: {{ printf "%s.%s.svc.%s" (include "gateway.headlessName" $root) $root.Release.Namespace $root.Values.clusterDomain | quote }}
- name: FLUXER_GATEWAY_CLUSTER_DISCOVERY_NODE_BASENAME
  value: fluxer_gateway
{{- include "gateway.envList" . }}
{{- end }}

{{- define "gateway.pod" -}}
{{- $root := .root }}
{{- $w := .w -}}
metadata:
  labels:
    {{- include "gateway.labels" . | nindent 4 }}
  {{- with include "gateway.podAnnotations" . }}
  annotations:
    {{- . | nindent 4 }}
  {{- end }}
spec:
  {{- with include "gateway.pick" (dict "root" $root "w" $w "key" "affinity") }}
  affinity:
    {{- . | nindent 4 }}
  {{- end }}
  {{- with include "gateway.pick" (dict "root" $root "w" $w "key" "imagePullSecrets") }}
  imagePullSecrets:
    {{- . | nindent 4 }}
  {{- end }}
  {{- with include "gateway.pick" (dict "root" $root "w" $w "key" "nodeSelector") }}
  nodeSelector:
    {{- . | nindent 4 }}
  {{- end }}
  {{- with include "gateway.pick" (dict "root" $root "w" $w "key" "tolerations") }}
  tolerations:
    {{- . | nindent 4 }}
  {{- end }}
  {{- with include "gateway.topologySpreadConstraints" . }}
  topologySpreadConstraints:
    {{- . | nindent 4 }}
  {{- end }}
  {{- with include "gateway.pick" (dict "root" $root "w" $w "key" "podSecurityContext") }}
  securityContext:
    {{- . | nindent 4 }}
  {{- end }}
  {{- if not (kindIs "invalid" $w.terminationGracePeriodSeconds) }}
  terminationGracePeriodSeconds: {{ $w.terminationGracePeriodSeconds }}
  {{- end }}
  containers:
  - name: gateway
    image: {{ include "gateway.image" . }}
    imagePullPolicy: {{ ($w.image | default dict).pullPolicy | default $root.Values.image.pullPolicy }}
    env:
      {{- include "gateway.env" . | trim | nindent 6 }}
    {{- with include "gateway.envFrom" . }}
    envFrom:
      {{- . | nindent 6 }}
    {{- end }}
    {{- with $w.lifecycle }}
    lifecycle:
      {{- toYaml . | nindent 6 }}
    {{- end }}
    ports:
    - name: http
      containerPort: 8080
      protocol: TCP
    - name: epmd
      containerPort: 4369
      protocol: TCP
    - name: erl-dist
      containerPort: 8081
      protocol: TCP
    {{- with include "gateway.probes" . | trim }}
    {{- . | nindent 4 }}
    {{- end }}
    {{- with $w.resources }}
    resources:
      {{- toYaml . | nindent 6 }}
    {{- end }}
    {{- with include "gateway.pick" (dict "root" $root "w" $w "key" "securityContext") }}
    securityContext:
      {{- . | nindent 6 }}
    {{- end }}
    {{- with $w.extraVolumeMounts }}
    volumeMounts:
      {{- toYaml . | nindent 6 }}
    {{- end }}
  {{- with $w.extraVolumes }}
  volumes:
    {{- toYaml . | nindent 4 }}
  {{- end }}
{{- end }}

{{- define "gateway.pdb" -}}
{{- with .w.pdb }}
---
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: {{ $.name }}-pdb
  namespace: {{ $.root.Release.Namespace }}
  labels:
    {{- include "gateway.labels" $ | nindent 4 }}
spec:
  {{- if not (kindIs "invalid" .minAvailable) }}
  minAvailable: {{ .minAvailable }}
  {{- end }}
  {{- if not (kindIs "invalid" .maxUnavailable) }}
  maxUnavailable: {{ .maxUnavailable }}
  {{- end }}
  selector:
    matchLabels:
      {{- include "gateway.selectorLabels" $ | nindent 6 }}
{{- end }}
{{- end }}

{{- define "gateway.hpa" -}}
{{- with .w.hpa }}
---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: {{ $.name }}
  namespace: {{ $.root.Release.Namespace }}
  labels:
    {{- include "gateway.labels" $ | nindent 4 }}
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: {{ $.name }}
  minReplicas: {{ required (printf "%s.hpa.minReplicas is required" $.name) .minReplicas }}
  maxReplicas: {{ required (printf "%s.hpa.maxReplicas is required" $.name) .maxReplicas }}
  {{- if not (kindIs "invalid" .targetCPUUtilizationPercentage) }}
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: {{ .targetCPUUtilizationPercentage }}
  {{- end }}
  {{- with .behavior }}
  behavior:
    {{- toYaml . | nindent 4 }}
  {{- end }}
{{- end }}
{{- end }}
