{{- define "fluxer-media-proxy.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end }}

{{- define "fluxer-media-proxy.selectorLabels" -}}
app.kubernetes.io/name: {{ .name }}
app.kubernetes.io/instance: {{ .root.Release.Name }}
{{- end }}

{{- define "fluxer-media-proxy.labels" -}}
{{ include "fluxer-media-proxy.selectorLabels" . }}
app.kubernetes.io/component: {{ include "fluxer-media-proxy.mode" . }}
app.kubernetes.io/part-of: fluxer
app.kubernetes.io/managed-by: {{ .root.Release.Service }}
helm.sh/chart: {{ include "fluxer-media-proxy.chart" .root }}
{{- end }}

{{- define "fluxer-media-proxy.image" -}}
{{- $g := .root.Values.image -}}
{{- $i := .w.image | default dict -}}
{{- $repo := $i.repository | default (printf "%s/%s" $g.registry ($i.name | default "fluxer-media-proxy")) -}}
{{- $tag := $i.tag | default $g.tag -}}
{{- if $i.digest -}}
{{- printf "%s:%s@%s" $repo $tag $i.digest | quote -}}
{{- else -}}
{{- printf "%s:%s" $repo $tag | quote -}}
{{- end -}}
{{- end }}

{{- define "fluxer-media-proxy.pick" -}}
{{- $v := ternary (get .w .key) (get .root.Values .key) (hasKey .w .key) -}}
{{- if $v }}
{{- toYaml $v }}
{{- end }}
{{- end }}

{{- define "fluxer-media-proxy.mode" -}}
{{- $mode := required (printf "workloads.%s.mode is required" .name) .w.mode -}}
{{- if not (has $mode (list "mp" "static" "upload" "relay")) -}}
{{- fail (printf "workloads.%s.mode must be mp, static, upload or relay" .name) -}}
{{- end -}}
{{- $mode -}}
{{- end }}

{{- define "fluxer-media-proxy.envValue" -}}
{{- if and (kindIs "float64" .) (eq . (float64 (int64 .))) -}}
{{- int64 . | toString -}}
{{- else -}}
{{- toString . -}}
{{- end -}}
{{- end }}

{{- define "fluxer-media-proxy.mergeEnv" -}}
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

{{- define "fluxer-media-proxy.topologySpreadConstraints" -}}
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

{{- define "fluxer-media-proxy.pdb" -}}
{{- $out := dict -}}
{{- range $k := list "minAvailable" "maxUnavailable" -}}
{{- if and (hasKey $ $k) (not (kindIs "invalid" (index $ $k))) -}}
{{- $_ := set $out $k (index $ $k) -}}
{{- end -}}
{{- end -}}
{{- toYaml $out -}}
{{- end }}
