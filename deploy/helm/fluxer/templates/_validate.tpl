{{- define "fluxer.validate" -}}
{{- if not (hasKey .Values "fluxerValidated") }}
{{- $_ := set .Values "fluxerValidated" true }}
{{- $v := .Values }}
{{- if not (regexMatch "^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$" ($v.domain | default "")) }}
{{- fail "domain must be set to the lowercase public hostname of the instance, for example --set domain=chat.example.com" }}
{{- end }}
{{- if not (has $v.scheme (list "http" "https")) }}
{{- fail "scheme must be http or https" }}
{{- end }}
{{- if and $v.publicOrigin (not (regexMatch "^https?://[^/]+$" (trimSuffix "/" $v.publicOrigin))) }}
{{- fail "publicOrigin must be a scheme, host and optional port, for example https://chat.example.com" }}
{{- end }}
{{- if not (regexMatch "^/[^/].*[^/]$|^/[^/]$" $v.adminBasePath) }}
{{- fail "adminBasePath must start with a slash and must not end with one, for example /admin" }}
{{- end }}
{{- $extra := $v.secrets.extra | default dict }}
{{- if and (not $v.postgres.enabled) (not $v.postgres.external.host) (not (get $extra "FLUXER_POSTGRES_URL")) (not $v.secrets.existingSecret) }}
{{- fail "postgres.enabled is false, so set postgres.external.host or secrets.extra.FLUXER_POSTGRES_URL" }}
{{- end }}
{{- if and (not $v.valkey.enabled) (not $v.valkey.external.url) (not (get $extra "FLUXER_KV_URL")) (not $v.secrets.existingSecret) }}
{{- fail "valkey.enabled is false, so set valkey.external.url or secrets.extra.FLUXER_KV_URL" }}
{{- end }}
{{- if and (not $v.nats.enabled) (not $v.nats.external.url) }}
{{- fail "nats.enabled is false, so set nats.external.url" }}
{{- end }}
{{- if and (not $v.meilisearch.enabled) (not $v.meilisearch.external.url) }}
{{- fail "meilisearch.enabled is false, so set meilisearch.external.url" }}
{{- end }}
{{- if and (not $v.seaweedfs.enabled) (not $v.s3.endpoint) }}
{{- fail "seaweedfs.enabled is false, so set s3.endpoint to your S3 store" }}
{{- end }}
{{- if gt (int $v.nats.replicas) 1 }}
{{- if lt (int $v.nats.replicas) 3 }}
{{- fail "nats.replicas must be 1, or 3 or more for a JetStream cluster" }}
{{- end }}
{{- end }}
{{- if gt (int (include "fluxer.replicas" (dict "w" $v.worker))) 1 }}
{{- fail "worker.replicas must be 1, because the worker runs the cron scheduler" }}
{{- end }}
{{- if not (has $v.livekit.expose (list "hostNetwork" "loadBalancer" "nodePort")) }}
{{- fail "livekit.expose must be hostNetwork, loadBalancer or nodePort" }}
{{- end }}
{{- if and $v.livekit.enabled $v.livekit.external.url }}
{{- fail "livekit.external.url names a LiveKit server of your own, so set livekit.enabled to false" }}
{{- end }}
{{- if and $v.livekit.external.url (not (regexMatch "^(wss?|https?)://[^/]+" ($v.livekit.external.publicUrl | default ""))) }}
{{- fail "livekit.external.url is set, so set livekit.external.publicUrl to the address browsers use to reach that LiveKit server, for example wss://livekit.example.com" }}
{{- end }}
{{- if and $v.livekit.enabled (eq $v.livekit.expose "loadBalancer") (not $v.livekit.nodeIP) }}
{{- fail "livekit.expose is loadBalancer, so reserve a public address before installing, set livekit.nodeIP to it, and pin the Service livekit-rtc to it with livekit.loadBalancerIP or livekit.serviceAnnotations. Use hostNetwork or nodePort where a load balancer cannot serve TCP and UDP on one address" }}
{{- end }}
{{- if not (has $v.edge.expose (list "ingress" "gateway" "loadBalancer" "none")) }}
{{- fail "edge.expose must be ingress, gateway, loadBalancer or none" }}
{{- end }}
{{- if not (has $v.edge.ingress.tls.mode (list "certManager" "existingSecret" "none")) }}
{{- fail "edge.ingress.tls.mode must be certManager, existingSecret or none" }}
{{- end }}
{{- if and (eq $v.edge.expose "gateway") (not $v.edge.gateway.parentRefs) }}
{{- fail "edge.expose is gateway, so set edge.gateway.parentRefs to the Gateway the route attaches to" }}
{{- end }}
{{- if and (eq $v.edge.expose "loadBalancer") (ne $v.scheme "https") }}
{{- fail "edge.expose loadBalancer serves HTTPS itself, so scheme must be https" }}
{{- end }}
{{- if and $v.networkPolicy.enabled $v.networkPolicy.ingressNamespaceSelector $v.livekit.enabled (eq $v.livekit.expose "hostNetwork") (not $v.livekit.nodeCIDRs) }}
{{- fail "networkPolicy.ingressNamespaceSelector limits who reaches the edge, and LiveKit on the host network sends its webhook to the edge from a node address. Set livekit.nodeCIDRs to the address of the LiveKit node, for example 10.0.0.12/32" }}
{{- end }}

{{- $placeholder := "(?i)change[_-]?me" }}
{{- $secretName := "_(PASSWORD|SECRET|TOKEN|COOKIE|KEY|KEYS|KEY_BASE|SECRETS?_BASE64)$" }}
{{- $credentialUrl := "://[^/@ ]*:[^/@ ]*@" }}
{{- $plain := dict "env" ($v.env | default dict) "appProxy.config" ($v.appProxy.config | default dict) "edge.env" ($v.edge.env | default dict) }}
{{- range $name := list "api" "worker" "gateway" "mediaProxy" "push" "admin" "appProxy" "staticProxy" }}
{{- $_ := set $plain (printf "%s.env" $name) ((get $v $name).env | default dict) }}
{{- end }}
{{- range $name, $svc := $v.svc }}
{{- range $part := list "router" "shard" }}
{{- $_ := set $plain (printf "svc.%s.%s.env" $name $part) ((get ($svc | default dict) $part | default dict).env | default dict) }}
{{- end }}
{{- end }}
{{- range $where, $map := $plain }}
{{- range $k, $val := $map }}
{{- $s := include "fluxer.string" ($val | default "") }}
{{- if regexMatch $placeholder $s }}
{{- fail (printf "%s.%s is a placeholder. Remove it or set a real value" $where $k) }}
{{- end }}
{{- if or (hasKey $v.secrets.values $k) (regexMatch $secretName $k) }}
{{- fail (printf "%s.%s is a secret and would be stored in plain text. Set it in secrets.extra instead, or reference a Secret of your own from extraEnv" $where $k) }}
{{- end }}
{{- if regexMatch $credentialUrl $s }}
{{- fail (printf "%s.%s is a URL that holds a password and would be stored in plain text. Set it in secrets.extra instead" $where $k) }}
{{- end }}
{{- end }}
{{- end }}
{{- range $k, $val := $v.secrets.values }}
{{- if regexMatch $placeholder (include "fluxer.string" ($val | default "")) }}
{{- fail (printf "secrets.values.%s is a placeholder. Leave it empty to generate a value, or set a real one" $k) }}
{{- end }}
{{- end }}
{{- range $k, $val := $extra }}
{{- if regexMatch $placeholder (include "fluxer.string" ($val | default "")) }}
{{- fail (printf "secrets.extra.%s is a placeholder. Remove it or set a real value" $k) }}
{{- end }}
{{- end }}

{{- $connected := false }}
{{- if $v.clusterChecks }}
{{- $connected = or (lookup "v1" "Namespace" "" .Release.Namespace) (lookup "v1" "Namespace" "" "kube-system") | not | not }}
{{- end }}
{{- $data := include "fluxer.secretData" . | fromJson }}
{{- $secret := include "fluxer.secretName" . }}
{{- if $v.secrets.existingSecret }}
{{- if and $connected (not $data) }}
{{- fail (printf "secrets.existingSecret names the Secret %s, which does not exist in namespace %s. Create it first with every key listed under secrets.values" $secret .Release.Namespace) }}
{{- end }}
{{- if $data }}
{{- $missing := list }}
{{- range $k := include "fluxer.requiredSecretKeys" . | fromJsonArray }}
{{- if not (get $data $k) }}
{{- $missing = append $missing $k }}
{{- end }}
{{- end }}
{{- with $missing }}
{{- fail (printf "Secret %s is missing these keys: %s" $secret (join ", " .)) }}
{{- end }}
{{- $config := include "fluxer.configData" . | fromYaml }}
{{- if $v.valkey.enabled }}
{{- $_ := set $config "FLUXER_KV_URL" "the bundled Valkey" }}
{{- end }}
{{- $shadowed := list }}
{{- range $k := keys $config | sortAlpha }}
{{- if and (hasKey $data $k) (ne (get $data $k) (get $config $k)) }}
{{- $shadowed = append $shadowed $k }}
{{- end }}
{{- end }}
{{- with $shadowed }}
{{- fail (printf "Secret %s holds settings the chart sets itself, and the two disagree: %s. Remove them from the Secret. To keep an outside service, turn the bundled one off, as the External services section of the Kubernetes page describes" $secret (join ", " .)) }}
{{- end }}
{{- end }}
{{- end }}
{{- if $data }}
{{- range $k, $val := $data }}
{{- if regexMatch $placeholder $val }}
{{- fail (printf "%s in the Secret %s is a placeholder. Set a real value" $k $secret) }}
{{- end }}
{{- end }}
{{- with get $data "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64" }}
{{- if or (not (regexMatch "^[A-Za-z0-9+/]+={0,2}$" .)) (lt (len (b64dec .)) 32) }}
{{- fail "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must be standard base64 of at least 32 bytes, for example the output of openssl rand -base64 32" }}
{{- end }}
{{- end }}
{{- with get $data "FLUXER_ACTIVITYPUB_SIGNING_KEY_BASE64" }}
{{- if or (not (regexMatch "^[A-Za-z0-9+/]+={0,2}$" .)) (lt (len (b64dec .)) 256) }}
{{- fail "FLUXER_ACTIVITYPUB_SIGNING_KEY_BASE64 must be the standard base64 of a DER RSA private key, for example the output of openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -outform DER | base64 | tr -d '\\n'" }}
{{- end }}
{{- end }}
{{- $long := list "FLUXER_SUDO_MODE_SECRET" "FLUXER_CONNECTION_INITIATION_SECRET" "FLUXER_PROFILE_PSEUDONYM_SECRET" "FLUXER_GATEWAY_RPC_AUTH_TOKEN" "FLUXER_ERLANG_COOKIE" "FLUXER_MEDIA_PROXY_SECRET_KEY" "FLUXER_ADMIN_SECRET_KEY_BASE" "FLUXER_ADMIN_OAUTH_CLIENT_SECRET" "FLUXER_SETUP_TOKEN" }}
{{- if ne (include "fluxer.livekitMode" .) "off" }}
{{- $long = append $long "FLUXER_LIVEKIT_API_SECRET" }}
{{- end }}
{{- if $v.valkey.enabled }}
{{- $long = append $long "FLUXER_VALKEY_PASSWORD" }}
{{- end }}
{{- if $v.nats.enabled }}
{{- $long = append $long "FLUXER_NATS_AUTH_TOKEN" }}
{{- end }}
{{- range $k := $long }}
{{- if and (hasKey $data $k) (lt (len (get $data $k)) 32) }}
{{- fail (printf "%s must be at least 32 characters, for example the output of openssl rand -hex 32" $k) }}
{{- end }}
{{- end }}
{{- if and $v.meilisearch.enabled (hasKey $data "FLUXER_SEARCH_API_KEY") (lt (len (get $data "FLUXER_SEARCH_API_KEY")) 16) }}
{{- fail "FLUXER_SEARCH_API_KEY must be at least 16 characters, or the bundled Meilisearch refuses to start" }}
{{- end }}
{{- if eq (get $data "FLUXER_POSTGRES_PASSWORD" | default "") "fluxer" }}
{{- fail "FLUXER_POSTGRES_PASSWORD must not be fluxer, which the services refuse in production" }}
{{- end }}
{{- $plainOnly := list }}
{{- if $v.seaweedfs.enabled }}
{{- $plainOnly = concat $plainOnly (list "FLUXER_S3_ACCESS_KEY_ID" "FLUXER_S3_SECRET_ACCESS_KEY") }}
{{- end }}
{{- if $v.valkey.enabled }}
{{- $plainOnly = append $plainOnly "FLUXER_VALKEY_PASSWORD" }}
{{- end }}
{{- if $v.nats.enabled }}
{{- $plainOnly = append $plainOnly "FLUXER_NATS_AUTH_TOKEN" }}
{{- end }}
{{- range $k := $plainOnly }}
{{- if and (hasKey $data $k) (not (regexMatch "^[A-Za-z0-9]+$" (get $data $k))) }}
{{- fail (printf "%s must contain letters and digits only, because the bundled service takes it on a command line or in a URL" $k) }}
{{- end }}
{{- end }}
{{- $pub := get $data "FLUXER_VAPID_PUBLIC_KEY" | default "" }}
{{- $priv := get $data "FLUXER_VAPID_PRIVATE_KEY" | default "" }}
{{- if or $pub $priv }}
{{- if or (not (regexMatch "^[A-Za-z0-9_-]{87}$" $pub)) (not (regexMatch "^[A-Za-z0-9_-]{43}$" $priv)) }}
{{- fail "FLUXER_VAPID_PUBLIC_KEY and FLUXER_VAPID_PRIVATE_KEY must be an unpadded base64url P-256 pair of 87 and 43 characters" }}
{{- end }}
{{- end }}
{{- end }}

{{- if $connected }}
{{- if and (eq $v.edge.expose "ingress") (eq $v.edge.ingress.tls.mode "certManager") (not (.Capabilities.APIVersions.Has "cert-manager.io/v1")) }}
{{- fail "edge.ingress.tls.mode is certManager, but cert-manager is not installed. Install cert-manager, or set edge.ingress.tls.mode to existingSecret or none, or set edge.expose to loadBalancer so the edge obtains its own certificate" }}
{{- end }}
{{- if and (eq $v.edge.expose "gateway") (not (.Capabilities.APIVersions.Has "gateway.networking.k8s.io/v1")) }}
{{- fail "edge.expose is gateway, but the Gateway API is not installed in the cluster" }}
{{- end }}
{{- $emptyClass := and (eq $v.edge.expose "loadBalancer") (not $v.edge.loadBalancer.persistence.storageClassName) }}
{{- range $c := list $v.postgres $v.valkey $v.nats $v.meilisearch $v.seaweedfs }}
{{- if and $c.enabled (not $c.persistence.storageClassName) }}
{{- $emptyClass = true }}
{{- end }}
{{- end }}
{{- if $emptyClass }}
{{- $default := false }}
{{- range (lookup "storage.k8s.io/v1" "StorageClass" "" "").items | default list }}
{{- if eq (get (.metadata.annotations | default dict) "storageclass.kubernetes.io/is-default-class") "true" }}
{{- $default = true }}
{{- end }}
{{- end }}
{{- if not $default }}
{{- fail "The cluster has no default StorageClass, so the volumes would never bind. Install a storage provisioner and mark its class as default, or set persistence.storageClassName on postgres, valkey, nats, meilisearch and seaweedfs" }}
{{- end }}
{{- end }}
{{- if and (eq $v.edge.expose "ingress") (not $v.edge.ingress.className) }}
{{- $classes := (lookup "networking.k8s.io/v1" "IngressClass" "" "").items | default list }}
{{- if not $classes }}
{{- fail "The cluster has no IngressClass, so nothing would serve the Ingress. Install an ingress controller, or set edge.expose to loadBalancer, gateway or none. Set clusterChecks to false if your controller serves an Ingress without a class" }}
{{- end }}
{{- $defaults := list }}
{{- range $classes }}
{{- if eq (get (.metadata.annotations | default dict) "ingressclass.kubernetes.io/is-default-class") "true" }}
{{- $defaults = append $defaults .metadata.name }}
{{- end }}
{{- end }}
{{- if not $defaults }}
{{- $names := list }}
{{- range $classes }}
{{- $names = append $names .metadata.name }}
{{- end }}
{{- fail (printf "The cluster has no default IngressClass. Set edge.ingress.className to one of: %s" (join ", " $names)) }}
{{- end }}
{{- if gt (len $defaults) 1 }}
{{- fail (printf "The cluster marks more than one IngressClass as default (%s), so the Ingress may land on the wrong controller. Set edge.ingress.className" (join ", " $defaults)) }}
{{- end }}
{{- end }}
{{- end }}
{{- end }}
{{- end }}
