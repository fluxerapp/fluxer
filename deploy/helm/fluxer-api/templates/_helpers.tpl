{{- define "fluxer-api.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end }}

{{- define "fluxer-api.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer-api.labels" -}}
{{ include "fluxer-api.selectorLabels" . }}
app.kubernetes.io/component: {{ .component }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ include "fluxer-api.chart" .root }}
{{- end }}

{{- define "fluxer-api.image" -}}
{{- $g := .root.Values.image | default dict -}}
{{- $i := .w.image | default dict -}}
{{- $repo := $i.repository -}}
{{- if not $repo -}}
{{- $repo = printf "%s/%s" (required "image.registry is required" $g.registry) ($i.name | default "fluxer-api") -}}
{{- end -}}
{{- $tag := required "image.tag is required" ($i.tag | default $g.tag) -}}
{{- if $i.digest -}}
{{- printf "%s:%s@%s" $repo $tag $i.digest | quote -}}
{{- else -}}
{{- printf "%s:%s" $repo $tag | quote -}}
{{- end -}}
{{- end }}

{{- define "fluxer-api.pick" -}}
{{- $v := ternary (get .w .key) (get .root.Values .key) (hasKey .w .key) -}}
{{- if $v }}
{{- toYaml $v }}
{{- end }}
{{- end }}

{{- define "fluxer-api.str" -}}
{{- if and (kindIs "float64" .) (eq . (floor .)) -}}
{{- int64 . | toString | quote -}}
{{- else -}}
{{- toString . | quote -}}
{{- end -}}
{{- end }}

{{- define "fluxer-api.env" -}}
{{- $env := dict -}}
{{- range $k, $val := .root.Values.env | default dict }}
{{- $_ := set $env $k $val }}
{{- end }}
{{- range $k, $val := .w.env | default dict }}
{{- $_ := set $env $k $val }}
{{- end }}
{{- range $k, $val := $env }}
{{- if not (kindIs "invalid" $val) }}
- name: {{ $k }}
  value: {{ include "fluxer-api.str" $val }}
{{- end }}
{{- end }}
{{- with .w.buildVersion }}
- name: BUILD_VERSION
  value: {{ include "fluxer-api.str" . }}
{{- end }}
{{- with concat (.root.Values.extraEnv | default list) (.w.extraEnv | default list) }}
{{ toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer-api.topologySpread" -}}
{{- $tscs := ternary .w.topologySpreadConstraints .root.Values.topologySpreadConstraints (hasKey .w "topologySpreadConstraints") -}}
{{- range $tscs }}
{{- $c := deepCopy . }}
{{- if not $c.labelSelector }}
{{- $_ := set $c "labelSelector" (dict "matchLabels" (include "fluxer-api.selectorLabels" $ | fromYaml)) }}
{{- end }}
- {{- toYaml $c | nindent 2 }}
{{- end }}
{{- end }}

{{- define "fluxer-api.pdb" -}}
{{- with .w.pdb }}
---
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: {{ $.name }}-pdb
  namespace: {{ $.root.Release.Namespace }}
  labels:
    {{- include "fluxer-api.labels" $ | nindent 4 }}
spec:
  {{- toYaml . | nindent 2 }}
  selector:
    matchLabels:
      {{- include "fluxer-api.selectorLabels" $ | nindent 6 }}
{{- end }}
{{- end }}

{{- define "fluxer-api.hpa" -}}
{{- with .w.hpa }}
---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: {{ $.name }}
  namespace: {{ $.root.Release.Namespace }}
  labels:
    {{- include "fluxer-api.labels" $ | nindent 4 }}
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: {{ $.name }}
  minReplicas: {{ required (printf "%s.hpa.minReplicas is required" $.name) .minReplicas }}
  maxReplicas: {{ required (printf "%s.hpa.maxReplicas is required" $.name) .maxReplicas }}
  {{- with .targetCPUUtilizationPercentage }}
  metrics:
    - type: Resource
      resource:
        name: cpu
        target:
          type: Utilization
          averageUtilization: {{ . }}
  {{- end }}
  {{- with .behavior }}
  behavior:
    {{- toYaml . | nindent 4 }}
  {{- end }}
{{- end }}
{{- end }}

{{- define "fluxer-api.deployment" -}}
{{- $root := .root -}}
{{- $v := $root.Values -}}
{{- $w := .w -}}
{{- $envFrom := concat ($v.envFrom | default list) ($w.envFrom | default list) -}}
{{- $podAnnotations := merge (dict) ($w.podAnnotations | default dict) ($v.podAnnotations | default dict) -}}
{{- $wProbes := $w.probes | default dict -}}
{{- $gProbes := .probes | default dict -}}
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ .name }}
  namespace: {{ $root.Release.Namespace }}
  labels:
    {{- include "fluxer-api.labels" . | nindent 4 }}
spec:
  {{- if not $w.hpa }}
  replicas: {{ if kindIs "invalid" $w.replicas }}1{{ else }}{{ int $w.replicas }}{{ end }}
  {{- end }}
  {{- if not (kindIs "invalid" $w.minReadySeconds) }}
  minReadySeconds: {{ int $w.minReadySeconds }}
  {{- end }}
  selector:
    matchLabels:
      {{- include "fluxer-api.selectorLabels" . | nindent 6 }}
  {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "strategy") }}
  strategy:
    {{- . | nindent 4 }}
  {{- end }}
  template:
    metadata:
      labels:
        {{- include "fluxer-api.labels" . | nindent 8 }}
      {{- with $podAnnotations }}
      annotations:
        {{- toYaml . | nindent 8 }}
      {{- end }}
    spec:
      {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "imagePullSecrets") }}
      imagePullSecrets:
        {{- . | nindent 8 }}
      {{- end }}
      {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "podSecurityContext") }}
      securityContext:
        {{- . | nindent 8 }}
      {{- end }}
      {{- if not (kindIs "invalid" $w.terminationGracePeriodSeconds) }}
      terminationGracePeriodSeconds: {{ int $w.terminationGracePeriodSeconds }}
      {{- end }}
      {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "nodeSelector") }}
      nodeSelector:
        {{- . | nindent 8 }}
      {{- end }}
      {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "affinity") }}
      affinity:
        {{- . | nindent 8 }}
      {{- end }}
      {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "tolerations") }}
      tolerations:
        {{- . | nindent 8 }}
      {{- end }}
      {{- with include "fluxer-api.topologySpread" . | trim }}
      topologySpreadConstraints:
        {{- . | nindent 8 }}
      {{- end }}
      containers:
        - name: {{ .name }}
          image: {{ include "fluxer-api.image" . }}
          imagePullPolicy: {{ ($w.image | default dict).pullPolicy | default ($v.image | default dict).pullPolicy | default "IfNotPresent" }}
          {{- with .command }}
          command:
            {{- toYaml . | nindent 12 }}
          {{- end }}
          {{- with include "fluxer-api.env" . | trim }}
          env:
            {{- . | nindent 12 }}
          {{- end }}
          {{- with $envFrom }}
          envFrom:
            {{- toYaml . | nindent 12 }}
          {{- end }}
          ports:
            - name: http
              containerPort: 8080
          {{- with $w.lifecycle }}
          lifecycle:
            {{- toYaml . | nindent 12 }}
          {{- end }}
          {{- range $probe := list "startup" "liveness" "readiness" }}
          {{- with hasKey $wProbes $probe | ternary (get $wProbes $probe) (get $gProbes $probe) }}
          {{ $probe }}Probe:
            {{- toYaml . | nindent 12 }}
          {{- end }}
          {{- end }}
          {{- with $w.resources }}
          resources:
            {{- toYaml . | nindent 12 }}
          {{- end }}
          {{- with include "fluxer-api.pick" (dict "root" $root "w" $w "key" "securityContext") }}
          securityContext:
            {{- . | nindent 12 }}
          {{- end }}
          {{- with $w.extraVolumeMounts }}
          volumeMounts:
            {{- toYaml . | nindent 12 }}
          {{- end }}
      {{- with $w.extraVolumes }}
      volumes:
        {{- toYaml . | nindent 8 }}
      {{- end }}
{{- end }}
