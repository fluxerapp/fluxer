{{- define "fluxer-push.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer-push.labels" -}}
{{ include "fluxer-push.selectorLabels" . }}
app.kubernetes.io/component: {{ include "fluxer-push.mode" . }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ printf "%s-%s" .root.Chart.Name .root.Chart.Version | replace "+" "_" }}
{{- end }}

{{- define "fluxer-push.mode" -}}
{{- $mode := .w.mode | default "delivery" -}}
{{- if not (has $mode (list "delivery" "relay")) -}}
{{- fail (printf "workloads.%s.mode must be delivery or relay" .name) -}}
{{- end -}}
{{- $mode -}}
{{- end }}

{{- define "fluxer-push.port" -}}
{{- .w.port | default (ternary 8127 8126 (eq (include "fluxer-push.mode" .) "relay")) -}}
{{- end }}

{{- define "fluxer-push.image" -}}
{{- $global := .root.Values.image | default dict -}}
{{- $img := .w.image | default dict -}}
{{- $repo := $img.repository -}}
{{- if not $repo -}}
{{- $repo = printf "%s/%s" (required "image.registry is required" $global.registry) ($img.name | default "fluxer-push") -}}
{{- end -}}
{{- $ref := printf "%s:%s" $repo (include "fluxer-push.string" (required "image.tag is required" ($img.tag | default $global.tag))) -}}
{{- with $img.digest }}{{ $ref = printf "%s@%s" $ref . }}{{ end -}}
{{- $ref -}}
{{- end }}

{{- define "fluxer-push.string" -}}
{{- if and (kindIs "float64" .) (eq . (floor .)) -}}
{{- . | int64 | toString -}}
{{- else -}}
{{- . | toString -}}
{{- end -}}
{{- end }}

{{- define "fluxer-push.env" -}}
{{- $env := deepCopy (.root.Values.env | default dict) -}}
{{- range $k, $v := (.w.env | default dict) -}}
{{- if kindIs "invalid" $v -}}
{{- $_ := unset $env $k -}}
{{- else -}}
{{- $_ := set $env $k $v -}}
{{- end -}}
{{- end -}}
{{- if not (kindIs "invalid" .w.port) -}}
{{- $_ := set $env "FLUXER_PUSH_SERVICE_PORT" .w.port -}}
{{- end -}}
{{- if not (kindIs "invalid" .w.buildVersion) }}
- name: BUILD_VERSION
  value: {{ include "fluxer-push.string" .w.buildVersion | quote }}
{{- end }}
{{- range $k, $v := $env }}
{{- if not (kindIs "invalid" $v) }}
- name: {{ $k }}
  value: {{ include "fluxer-push.string" $v | quote }}
{{- end }}
{{- end }}
{{- with concat (.root.Values.extraEnv | default list) (.w.extraEnv | default list) }}
{{ toYaml . }}
{{- end }}
{{- end }}
