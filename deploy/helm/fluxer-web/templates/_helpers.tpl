{{- define "fluxer-web.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end }}

{{- define "fluxer-web.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer-web.labels" -}}
{{ include "fluxer-web.selectorLabels" . }}
app.kubernetes.io/component: web
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ include "fluxer-web.chart" .root }}
{{- end }}

{{- define "fluxer-web.image" -}}
{{- $g := .root.Values.image | default dict -}}
{{- $i := .w.image | default dict -}}
{{- $repo := $i.repository -}}
{{- if not $repo -}}
{{- $repo = printf "%s/%s" (required "image.registry is required" $g.registry) ($i.name | default (printf "fluxer-%s" .name)) -}}
{{- end -}}
{{- $tag := required "image.tag is required" ($i.tag | default $g.tag) -}}
{{- if $i.digest -}}
{{- printf "%s:%s@%s" $repo $tag $i.digest | quote -}}
{{- else -}}
{{- printf "%s:%s" $repo $tag | quote -}}
{{- end -}}
{{- end }}

{{- define "fluxer-web.pick" -}}
{{- $v := ternary (get .w .key) (get .root.Values .key) (hasKey .w .key) -}}
{{- if $v }}
{{- toYaml $v }}
{{- end }}
{{- end }}

{{- define "fluxer-web.str" -}}
{{- if and (kindIs "float64" .) (eq . (floor .)) -}}
{{- int64 . | toString | quote -}}
{{- else -}}
{{- toString . | quote -}}
{{- end -}}
{{- end }}

{{- define "fluxer-web.env" -}}
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
  value: {{ include "fluxer-web.str" $val }}
{{- end }}
{{- end }}
{{- with .w.buildVersion }}
- name: BUILD_VERSION
  value: {{ include "fluxer-web.str" . }}
{{- end }}
{{- with concat (.root.Values.extraEnv | default list) (.w.extraEnv | default list) }}
{{ toYaml . }}
{{- end }}
{{- end }}

{{- define "fluxer-web.topologySpread" -}}
{{- $tscs := ternary .w.topologySpreadConstraints .root.Values.topologySpreadConstraints (hasKey .w "topologySpreadConstraints") -}}
{{- range $tscs }}
{{- $c := deepCopy . }}
{{- if not $c.labelSelector }}
{{- $_ := set $c "labelSelector" (dict "matchLabels" (include "fluxer-web.selectorLabels" $ | fromYaml)) }}
{{- end }}
- {{- toYaml $c | nindent 2 }}
{{- end }}
{{- end }}
