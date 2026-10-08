{{- define "fluxer-svc.chart" -}}
{{ printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" }}
{{- end }}

{{- define "fluxer-svc.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer-svc.labels" -}}
{{ include "fluxer-svc.selectorLabels" . }}
app.kubernetes.io/component: {{ .mode }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ include "fluxer-svc.chart" .root }}
{{- end }}

{{- define "fluxer-svc.envValue" -}}
{{- if and (kindIs "float64" .) (eq . (float64 (int64 .))) -}}
{{- int64 . | toString -}}
{{- else -}}
{{- toString . -}}
{{- end -}}
{{- end }}

{{- define "fluxer-svc.mergeEnv" -}}
{{- $out := dict -}}
{{- range $layer := . -}}
{{- range $k, $v := ($layer | default dict) -}}
{{- if kindIs "invalid" $v -}}
{{- $_ := unset $out $k -}}
{{- else -}}
{{- $_ := set $out $k $v -}}
{{- end -}}
{{- end -}}
{{- end -}}
{{- toYaml $out -}}
{{- end }}

{{- define "fluxer-svc.topologySpreadConstraints" -}}
{{- $out := list -}}
{{- range .constraints -}}
{{- if .labelSelector -}}
{{- $out = append $out . -}}
{{- else -}}
{{- $out = append $out (merge (dict "labelSelector" (dict "matchLabels" $.selector)) .) -}}
{{- end -}}
{{- end -}}
{{- toYaml $out -}}
{{- end }}

{{- define "fluxer-svc.pdb" -}}
{{- $out := dict -}}
{{- range $k := list "minAvailable" "maxUnavailable" -}}
{{- if and (hasKey $ $k) (not (kindIs "invalid" (index $ $k))) -}}
{{- $_ := set $out $k (index $ $k) -}}
{{- end -}}
{{- end -}}
{{- toYaml $out -}}
{{- end }}

{{- define "fluxer-svc.config" -}}
{{- $v := .root.Values -}}
{{- $levels := list (index $v .mode) (index .svc .mode) -}}
{{- $c := dict "extraEnv" ($v.extraEnv | default list) "envFrom" ($v.envFrom | default list) "podAnnotations" (deepCopy ($v.podAnnotations | default dict)) "probes" (deepCopy ($v.probes | default dict)) "image" (deepCopy (.svc.image | default dict)) -}}
{{- range $k := list "imagePullSecrets" "podSecurityContext" "securityContext" "topologySpreadConstraints" "nodeSelector" "tolerations" "affinity" (ternary "updateStrategy" "strategy" (eq .mode "shard")) -}}
{{- $_ := set $c $k (index $v $k) -}}
{{- end -}}
{{- $envLayers := list $v.env -}}
{{- range $level := $levels -}}
{{- range $k, $x := ($level | default dict) -}}
{{- if eq $k "env" -}}
{{- $envLayers = append $envLayers $x -}}
{{- else if has $k (list "podAnnotations" "image") -}}
{{- $_ := set $c $k (mergeOverwrite (index $c $k) (deepCopy ($x | default dict))) -}}
{{- else if has $k (list "extraEnv" "envFrom") -}}
{{- $_ := set $c $k (concat (index $c $k) ($x | default list)) -}}
{{- else if eq $k "probes" -}}
{{- range $name, $p := ($x | default dict) -}}
{{- $_ := set $c.probes $name $p -}}
{{- end -}}
{{- else -}}
{{- $_ := set $c $k $x -}}
{{- end -}}
{{- end -}}
{{- end -}}
{{- $_ := set $c "env" (include "fluxer-svc.mergeEnv" $envLayers | fromYaml) -}}
{{- toYaml $c }}
{{- end }}

{{- define "fluxer-svc.image" -}}
{{- $g := .root.Values.image -}}
{{- $i := .c.image -}}
{{- $repo := $i.repository | default (printf "%s/%s" $g.registry ($i.name | default (printf "fluxer-%s" .service))) -}}
{{- $ref := printf "%s:%s" $repo ($i.tag | default $g.tag) -}}
{{- with $i.digest }}{{ $ref = printf "%s@%s" $ref . }}{{ end -}}
{{- $ref -}}
{{- end }}

{{- define "fluxer-svc.pod" -}}
{{- $v := .root.Values -}}
{{- $c := .c -}}
metadata:
  {{- with $c.podAnnotations }}
  annotations:
    {{- toYaml . | nindent 4 }}
  {{- end }}
  labels:
    {{- include "fluxer-svc.labels" . | nindent 4 }}
spec:
  {{- with $c.imagePullSecrets }}
  imagePullSecrets:
    {{- toYaml . | nindent 4 }}
  {{- end }}
  {{- with $c.podSecurityContext }}
  securityContext:
    {{- toYaml . | nindent 4 }}
  {{- end }}
  {{- if not (kindIs "invalid" $c.terminationGracePeriodSeconds) }}
  terminationGracePeriodSeconds: {{ $c.terminationGracePeriodSeconds | int64 }}
  {{- end }}
  {{- with $c.nodeSelector }}
  nodeSelector:
    {{- toYaml . | nindent 4 }}
  {{- end }}
  {{- with $c.tolerations }}
  tolerations:
    {{- toYaml . | nindent 4 }}
  {{- end }}
  {{- with $c.affinity }}
  affinity:
    {{- toYaml . | nindent 4 }}
  {{- end }}
  {{- with $c.topologySpreadConstraints }}
  topologySpreadConstraints:
    {{- include "fluxer-svc.topologySpreadConstraints" (dict "constraints" . "selector" (include "fluxer-svc.selectorLabels" $ | fromYaml)) | nindent 4 }}
  {{- end }}
  containers:
    - name: {{ .mode }}
      image: {{ include "fluxer-svc.image" . | quote }}
      imagePullPolicy: {{ $c.image.pullPolicy | default $v.image.pullPolicy }}
      env:
        - name: FLUXER_SVC_MODE
          value: {{ .mode | quote }}
        - name: FLUXER_SVC_NAME
          value: {{ .service | quote }}
        - name: FLUXER_SVC_SHARD_COUNT
          value: {{ .shardCount | quote }}
        - name: FLUXER_SVC_PORT
          value: {{ include "fluxer-svc.envValue" $v.port | quote }}
        {{- if not (kindIs "invalid" $c.buildVersion) }}
        - name: BUILD_VERSION
          value: {{ include "fluxer-svc.envValue" $c.buildVersion | quote }}
        {{- end }}
        {{- if eq .mode "shard" }}
        - name: POD_NAME
          valueFrom:
            fieldRef:
              apiVersion: v1
              fieldPath: metadata.name
        {{- end }}
        {{- range $name, $value := $c.env }}
        - name: {{ $name }}
          value: {{ include "fluxer-svc.envValue" $value | quote }}
        {{- end }}
        {{- with $c.extraEnv }}
        {{- toYaml . | nindent 8 }}
        {{- end }}
      {{- with $c.envFrom }}
      envFrom:
        {{- toYaml . | nindent 8 }}
      {{- end }}
      ports:
        - name: http
          containerPort: {{ $v.port }}
          protocol: TCP
      {{- with $c.lifecycle }}
      lifecycle:
        {{- toYaml . | nindent 8 }}
      {{- end }}
      {{- range $name := list "startup" "liveness" "readiness" }}
      {{- with index $c.probes $name }}
      {{ $name }}Probe:
        {{- toYaml . | nindent 8 }}
      {{- end }}
      {{- end }}
      {{- with $c.resources }}
      resources:
        {{- toYaml . | nindent 8 }}
      {{- end }}
      {{- with $c.securityContext }}
      securityContext:
        {{- toYaml . | nindent 8 }}
      {{- end }}
      {{- with $c.extraVolumeMounts }}
      volumeMounts:
        {{- toYaml . | nindent 8 }}
      {{- end }}
  {{- with $c.extraVolumes }}
  volumes:
    {{- toYaml . | nindent 4 }}
  {{- end }}
{{- end }}
