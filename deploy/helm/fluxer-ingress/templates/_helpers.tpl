{{- define "fluxer-ingress.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end }}

{{- define "fluxer-ingress.labels" -}}
app.kubernetes.io/name: {{ .Chart.Name }}
app.kubernetes.io/instance: {{ .Release.Name }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .Release.Service }}
helm.sh/chart: {{ include "fluxer-ingress.chart" . }}
{{- end }}

{{- define "fluxer-ingress.annotationKey" -}}
{{- if or (contains "/" .key) (not .prefix) -}}
{{- .key -}}
{{- else -}}
{{- printf "%s/%s" .prefix .key -}}
{{- end -}}
{{- end }}

{{- define "fluxer-ingress.string" -}}
{{- if and (kindIs "float64" .) (eq . (floor .)) -}}
{{- . | int64 | toString -}}
{{- else -}}
{{- . | toString -}}
{{- end -}}
{{- end }}
