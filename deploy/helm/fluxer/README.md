# Fluxer Helm chart

This chart runs Fluxer as one pod from the `ghcr.io/fluxerapp/fluxer` image. fluxerd runs everything inside it, PostgreSQL, the API, the gateway, search, voice and the web front. The chart needs no subcharts, CronJobs or RBAC and creates no Secrets. `helm template`, Argo CD and Flux CD render it the same way as `helm install`.

## Install

You need Kubernetes 1.29 or newer, a default StorageClass and a domain name or an IP address for Fluxer.

```sh
helm install fluxer oci://ghcr.io/fluxerapp/charts/fluxer \
  --namespace fluxer --create-namespace \
  --set domain=chat.yourdomain.org \
  --set ingress.annotations."cert-manager\.io/cluster-issuer"=letsencrypt
kubectl -n fluxer rollout status statefulset/fluxer --timeout 15m
kubectl -n fluxer exec fluxer-0 -- fluxerd setup-token
```

Open `https://chat.yourdomain.org` and use the setup token to create the first admin account.

The commands on this page use `<fullname>` for the name of the StatefulSet, its Services and its claim. It is `<release>-fluxer`, or the release name alone when it contains `fluxer`. A release named `fluxer` gives `fluxer`, and a release named `chat` gives `chat-fluxer`.

The pod runs as user and group 65532. On OpenShift, grant the `nonroot-v2` SCC to the namespace's service account, because `restricted-v2` refuses a fixed user ID.

## Web traffic

`edge.mode` decides how traffic gets to Fluxer.

- `proxy` is the default for a domain name. Fluxer serves plain HTTP on port 8080 and an Ingress or a Gateway API HTTPRoute in front of it handles HTTPS. Fluxer trusts `X-Forwarded-For` from the private ranges your ingress controller uses, set by `edge.trustedProxies`. The Ingress uses the controller's default class unless you set `ingress.className`. Add your cert-manager issuer as an annotation, or put your own certificate in the Secret set in `ingress.tls.secretName`.
- `bundled` is the default when `domain` is an IP address. With an IP address and both `ingress.enabled` and `httpRoute.enabled` off, it stays `proxy` so you can route port 8080 yourself. `bundled` puts Fluxer on ports 80 and 443 of a LoadBalancer Service and Fluxer gets its own Let's Encrypt certificate, with no ingress controller or cert-manager. Point the domain at the Service's address. To run on an IP address without a domain, set `domain` to it, and the Service asks the load balancer for that address unless `edge.loadBalancer.loadBalancerIP` says otherwise. For an IPv6 address both Services ask for dual-stack. k3s ServiceLB doesn't take a requested address and uses the addresses of its nodes. A public IPv4 or IPv6 address gets a Let's Encrypt certificate. A private one gets a certificate Fluxer signs itself, which each device has to trust, from `http://<address>/fluxer-root.crt`. An Ingress or HTTPRoute can't route an IP address, so the chart refuses one in `proxy` mode. Turn on `edge.loadBalancer.http3` when your load balancer takes TCP and UDP on one Service. On a single node with no load balancer, set `edge.expose` to `hostPort` to open ports 80 and 443 on the node itself.

Uploads and WebSockets go through the same address. ingress-nginx, which stopped getting releases in March 2026, needs its 1 MiB body size limit raised. Traefik 3 gives each request 60 seconds to arrive in full, so raise `respondingTimeouts.readTimeout` on its entry points for slow uploads. For Envoy Gateway and other Gateway API implementations, set `httpRoute.enabled` and `httpRoute.parentRefs` and turn off `ingress.enabled`.

## Voice

`voice.mode` decides where calls run.

- `bundled`, the default, runs LiveKit in the same pod, with signalling through the web address and media on TCP 7881 and UDP 7882.
- `external` runs no LiveKit in the pod and opens no voice ports. Run LiveKit yourself, with its Docker image, its binary or its own Helm chart, and add it under "Voice servers" in the admin panel with its endpoint, API key and API secret.
- `off` turns voice and video off.

With `bundled`, `voice.expose` decides how clients connect to the media ports.

- `hostPort`, the default, opens both ports on the node that runs the pod. Fluxer finds the node's public address over STUN. Only one pod per node can hold those ports. A second Fluxer release on the same node, or the running LiveKit of a 0.x release, leaves the new pod Pending. A namespace that enforces Pod Security `baseline` or `restricted` refuses host ports, and the pod never starts there. Use `loadBalancer` or `nodePort` in these cases.
- `loadBalancer` puts both ports on a second LoadBalancer Service, `<fullname>-voice`, which needs a load balancer that takes TCP and UDP on one Service. Set `voice.service.loadBalancerIP` or `voice.publicIP` to its external address.
- `nodePort` uses `voice.nodePorts.tcp` and `voice.nodePorts.udp` on every node, and voice tells clients those ports. Give each release on a cluster its own pair.

## Settings and secrets

Fluxer creates its own passwords and keys the first time it starts and keeps them on the data volume. The chart needs no Secret for them. Everything else is a setting with the same name as in `fluxer.env` for Docker and systemd, listed at <https://docs.fluxer.com/operator/settings/>. Put settings in `config`, and put passwords in a Secret you manage and set its name in `existingSecret`.

```yaml
domain: chat.yourdomain.org
config:
  FLUXER_EMAIL_ENABLED: true
  FLUXER_EMAIL_PROVIDER: smtp
  FLUXER_EMAIL_SMTP_HOST: smtp.yourdomain.org
  FLUXER_EMAIL_FROM_EMAIL: noreply@yourdomain.org
existingSecret: fluxer-mail
```

A ConfigMap is plain text, and the chart refuses a password or key in `config`. It also refuses settings that a value of its own sets, such as `FLUXER_DOMAIN`. A change to `config` restarts the pod. A change to the Secret doesn't. Restart it yourself with `kubectl rollout restart statefulset/<fullname>`.

To use your own PostgreSQL 14 or later, set `postgres.externalSecret` to a Secret with a connection URL for a database and role that already exist. A CloudNativePG cluster's `<cluster>-app` Secret works as it is. With the default pool sizes, Fluxer opens up to 39 connections to it.

## Admin accounts

When nobody can sign in as an admin, make a reset link for an account or create a new admin from the pod. Each command prints a link that sets a password. It works once for an hour.

```sh
kubectl -n fluxer exec fluxer-0 -- fluxerd users reset-link alex
kubectl -n fluxer exec fluxer-0 -- fluxerd users create alex --admin
```

## Storage and backups

One PersistentVolumeClaim, `data-<fullname>-0`, holds the database, uploads, the search index, certificates and the keys Fluxer created. Uninstalling the chart keeps it. Take a backup to your machine with

```sh
kubectl -n fluxer exec fluxer-0 -- fluxerd backup --out - > fluxer-backup.tar.zst
```

A VolumeSnapshot of the claim also works.

Set `backup.schedule` to `daily` or `weekly` for backups inside Fluxer, kept in `/var/lib/fluxer/backups`. Fluxer shows a maintenance page while it copies uploads and the search index. Turn on `backup.persistence.enabled` to keep the archives on a second volume, which the chart keeps after an uninstall.

The `persistence` values apply at install only. Kubernetes doesn't let a StatefulSet change its claim template, and `helm upgrade` fails on a change to them. To grow the volume on a StorageClass with `allowVolumeExpansion`, resize the claim and replace the StatefulSet without touching the pod, then upgrade with the new size. The third line reads the chart version you run, so the upgrade changes nothing else.

```sh
kubectl -n fluxer patch pvc data-<fullname>-0 -p '{"spec":{"resources":{"requests":{"storage":"50Gi"}}}}'
kubectl -n fluxer delete statefulset <fullname> --cascade=orphan
version=$(helm get metadata <release> -n fluxer | awk '/^VERSION:/ {print $2}')
helm upgrade <release> oci://ghcr.io/fluxerapp/charts/fluxer -n fluxer --version "$version" --reset-then-reuse-values --set persistence.size=50Gi
```

## Restore a backup

Restoring needs Fluxer stopped, and `maintenance` starts the pod without it. This works for an archive from this chart, from Docker or from systemd. The first line reads the chart version you run. A backup from a newer release only restores on that release or a newer one, and `fluxerd restore` then stops with the version it needs. Run the commands again with `version=<the version it names>` in place of the first line.

```sh
version=$(helm get metadata <release> -n fluxer | awk '/^VERSION:/ {print $2}')
helm upgrade <release> oci://ghcr.io/fluxerapp/charts/fluxer -n fluxer --version "$version" --reset-then-reuse-values --set maintenance=true
kubectl -n fluxer rollout status statefulset/<fullname>
kubectl -n fluxer exec -i <fullname>-0 -- fluxerd restore - < fluxer-backup.tar.zst
helm upgrade <release> oci://ghcr.io/fluxerapp/charts/fluxer -n fluxer --version "$version" --reset-then-reuse-values --set maintenance=false
```

On a new cluster, install the chart with `--set maintenance=true` and the same `domain`, restore, then upgrade with `maintenance=false`. Fluxer starts with the restored data.

## Upgrades and going back

Fluxer doesn't look for new releases by itself. Set `config.FLUXER_UPDATE_CHECK` to `"true"` and it asks https://fluxer.dev/releases.json once a day for the list of releases. The request holds nothing about your instance. When a newer release is out, the Updates page of the admin panel shows it with this command, with the new version in place of `<version>`.

```sh
helm upgrade fluxer oci://ghcr.io/fluxerapp/charts/fluxer -n fluxer --version <version> --reset-then-reuse-values
```

`--reset-then-reuse-values` needs Helm 3.14 or later, Helm 4 included. It takes the new chart's defaults and keeps the values you set, while `--reuse-values` leaves out any value a newer chart adds. When you set `image.tag` or `image.digest`, Fluxer stays on that image until you change it too. The command in the admin panel adds `--set image.tag=` with the new version, or `--set image.digest=` to clear the digest, when you set one. With Argo CD or Flux CD, change the chart version in your own files instead, and set `config.FLUXER_UPGRADE_COMMAND` to show your own steps in the admin panel, with `{version}` where the version goes.

The image tag follows the chart version. Fluxer backs up its database before it starts the newer release. Older code can't run on a newer database. After a `helm rollback` to an older release Fluxer stays up but not ready, and its log says which archive to restore.

```sh
kubectl -n fluxer logs fluxer-0
kubectl -n fluxer exec fluxer-0 -- fluxerd restore /var/lib/fluxer/backups/<archive>
```

Fluxer then starts in the same pod.

## Moving from the 0.x chart

The 0.x chart ran each part of Fluxer as its own workload and kept its data in PostgreSQL, NATS, Valkey and SeaweedFS. Upgrading a 0.x release with this chart would start an empty instance, and Helm refuses it. Argo CD and `helm template` can't check this. Never point an existing Argo CD application at this chart.

To move, install this chart under a new release name in the same namespace and copy the data with `fluxerd import`. It reads the 0.x database, Valkey, NATS streams and uploads over the cluster network, keeps the 0.x keys that stored data depends on, and writes one backup archive for `fluxerd restore`. The steps use `fluxer` for the 0.x release, `chat` for the new one and the namespace `fluxer`. The new volume needs room for the uploads and the database twice over.

Note how many replicas each 0.x Deployment runs, then stop the 0.x parts that write. This also frees the 0.x voice ports. Its databases keep running.

```sh
kubectl -n fluxer get deployment -l app.kubernetes.io/instance=fluxer \
  -o custom-columns=NAME:.metadata.name,REPLICAS:.spec.replicas --no-headers > fluxer-0x-replicas.txt
kubectl -n fluxer scale deployment -l app.kubernetes.io/instance=fluxer --replicas=0
```

The 0.x NetworkPolicy lets in only the 0.x pods. Let the new release in as well.

```sh
kubectl -n fluxer apply -f - <<'EOF'
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: fluxer-import
spec:
  podSelector:
    matchLabels:
      app.kubernetes.io/instance: fluxer
  policyTypes: [Ingress]
  ingress:
    - from:
        - podSelector:
            matchLabels:
              app.kubernetes.io/instance: chat
EOF
```

Write a values file with the same `domain`. Move the 0.x `env` values to `config`, and the `secrets.extra` values to a Secret named in `existingSecret`. Leave `postgres.externalSecret` empty, because `fluxerd restore` loads the import only into PostgreSQL in the pod. Then install with `maintenance` on, import and restore. If the 0.x release used `secrets.existingSecret`, name that Secret in place of `secret/fluxer-env secret/fluxer-unfurl-env`.

```sh
helm install chat oci://ghcr.io/fluxerapp/charts/fluxer -n fluxer -f values.yaml --set maintenance=true
kubectl -n fluxer rollout status statefulset/chat-fluxer
kubectl -n fluxer get secret/fluxer-env secret/fluxer-unfurl-env configmap/fluxer-config -o json \
  | kubectl -n fluxer exec -i chat-fluxer-0 -- fluxerd import -
kubectl -n fluxer exec chat-fluxer-0 -- fluxerd restore /var/lib/fluxer/backups/<archive>
```

`fluxerd import` prints the archive's path. If the import or the restore fails, the 0.x release is still installed. Start its parts again and remove `chat`.

```sh
while read -r name replicas; do kubectl -n fluxer scale deployment "$name" --replicas="$replicas"; done < fluxer-0x-replicas.txt
helm uninstall chat -n fluxer
kubectl -n fluxer delete networkpolicy fluxer-import
```

Once the restore has finished, remove the 0.x release, which keeps its volumes and its Secrets, and start Fluxer.

```sh
helm uninstall fluxer -n fluxer
kubectl -n fluxer delete networkpolicy fluxer-import
helm upgrade chat oci://ghcr.io/fluxerapp/charts/fluxer -n fluxer -f values.yaml --set maintenance=false
```

People sign in with their passwords as before. Search starts empty, so open "Search Index" in the admin panel and choose "Reindex everything".

To go back after that, uninstall `chat` and install the 0.x chart again under the name `fluxer` with its old values. It starts on its kept volumes, without what was written after the move. Once the new release has everything, delete the 0.x volumes and Secrets.

```sh
kubectl -n fluxer delete pvc -l app.kubernetes.io/instance=fluxer
kubectl -n fluxer delete secret fluxer-env fluxer-unfurl-env
```

## Parameters

| Name | Description | Default |
| --- | --- | --- |
| `domain` | The domain or IP address people open, without `https://`. Required | `""` |
| `image.repository` | The Fluxer image | `ghcr.io/fluxerapp/fluxer` |
| `image.tag` | The image tag. Empty uses the chart's appVersion | `""` |
| `image.digest` | Pins the image by digest, and wins over the tag | `""` |
| `image.pullPolicy` | Image pull policy | `IfNotPresent` |
| `imagePullSecrets` | Secrets for a private registry | `[]` |
| `nameOverride` | Used in place of the chart name | `""` |
| `fullnameOverride` | Replaces the whole resource name | `""` |
| `config` | Fluxer settings, with the same keys as `fluxer.env` | `{}` |
| `existingSecret` | A Secret whose keys become settings, for passwords and keys | `""` |
| `postgres.externalSecret` | A Secret with the URL of your own PostgreSQL 14 or later. Empty runs PostgreSQL in the pod | `""` |
| `postgres.externalSecretKey` | The key in that Secret | `uri` |
| `persistence.size` | Size of the data volume, at install | `20Gi` |
| `persistence.storageClass` | StorageClass of the data volume, at install. Empty uses the default | `""` |
| `persistence.annotations` | Annotations on the data claim, at install | `{}` |
| `persistence.existingClaim` | A PersistentVolumeClaim to use in place of the chart's own, at install | `""` |
| `maintenance` | Starts the pod without Fluxer, for a restore | `false` |
| `edge.mode` | `proxy` behind an Ingress or HTTPRoute, or `bundled` on a LoadBalancer with its own certificate. Empty picks `bundled` when `domain` is an IP address and `proxy` for a domain name. With an IP address and both `ingress.enabled` and `httpRoute.enabled` off, it stays `proxy` so you can route port 8080 yourself | `""` |
| `edge.trustedProxies` | Addresses allowed to set `X-Forwarded-For` in proxy mode | `private_ranges` |
| `edge.expose` | `loadBalancer` puts ports 80 and 443 on a LoadBalancer Service in bundled mode, `hostPort` opens them on the node | `loadBalancer` |
| `edge.loadBalancer.loadBalancerIP` | A fixed address for the bundled LoadBalancer Service. Empty asks for `domain` when it is an IP address | `""` |
| `edge.loadBalancer.http3` | Also publish UDP 443 for HTTP/3 in bundled mode | `false` |
| `ingress.enabled` | Create an Ingress in proxy mode | `true` |
| `ingress.className` | The IngressClass. Empty uses the cluster default | `""` |
| `ingress.annotations` | Annotations on the Ingress, such as a cert-manager issuer | `{}` |
| `ingress.tls.enabled` | Serve HTTPS on the Ingress | `true` |
| `ingress.tls.secretName` | The certificate Secret. Empty uses `<fullname>-tls` | `""` |
| `httpRoute.enabled` | Create a Gateway API HTTPRoute in proxy mode | `false` |
| `httpRoute.annotations` | Annotations on the HTTPRoute | `{}` |
| `httpRoute.parentRefs` | The Gateways the route attaches to | `[]` |
| `httpRoute.timeouts.request` | The request timeout. `0s` turns it off for long-lived connections | `0s` |
| `voice.mode` | `bundled` LiveKit in the pod, `external` LiveKit servers, or `off` | `bundled` |
| `voice.expose` | `hostPort`, `loadBalancer` or `nodePort` | `hostPort` |
| `voice.publicIP` | The public address clients send media to. Empty uses `voice.service.loadBalancerIP` with `loadBalancer`, then `domain` when it is an IP address and the edge and voice both use `hostPort`, then STUN | `""` |
| `voice.nodePorts.tcp` | The TCP node port with `nodePort` | `30881` |
| `voice.nodePorts.udp` | The UDP node port with `nodePort` | `30882` |
| `voice.service.annotations` | Annotations on the voice Service | `{}` |
| `voice.service.loadBalancerIP` | A fixed address for the voice LoadBalancer Service | `""` |
| `backup.schedule` | `daily` or `weekly` backups inside Fluxer. Empty turns them off | `""` |
| `backup.keep` | How many scheduled backups to keep | `7` |
| `backup.hour` | The hour of the day, in UTC, that scheduled backups run | `3` |
| `backup.persistence.enabled` | Keep the archives on a second volume | `false` |
| `backup.persistence.size` | Size of the backup volume | `20Gi` |
| `backup.persistence.storageClass` | StorageClass of the backup volume | `""` |
| `backup.persistence.existingClaim` | A PersistentVolumeClaim for the archives | `""` |
| `resources` | CPU and memory for the pod. Fluxer sizes its threads from the CPUs it can use | 250m and 1Gi requested, 3Gi limit |
| `shmSize` | Shared memory for PostgreSQL | `256Mi` |
| `terminationGracePeriodSeconds` | Time for Fluxer to stop every part and PostgreSQL last | `130` |
| `networkPolicy.enabled` | Create a NetworkPolicy for traffic into the pod | `false` |
| `networkPolicy.from` | Peers allowed to connect to port 8080 in proxy mode. Empty allows everyone | `[]` |
| `podAnnotations` | Annotations on the pod | `{}` |
| `podLabels` | Labels on the pod | `{}` |
| `priorityClassName` | The pod's PriorityClass | `""` |
| `nodeSelector` | Node labels the pod must run on | `{}` |
| `tolerations` | Tolerations for the pod | `[]` |
| `affinity` | Affinity rules for the pod | `{}` |
| `service.annotations` | Annotations on the Service in front of Fluxer, in both edge modes | `{}` |
| `extraEnv` | More environment variables for the container | `[]` |
| `extraVolumes` | More volumes for the pod | `[]` |
| `extraVolumeMounts` | More mounts for the container | `[]` |
| `extraContainers` | More containers in the pod | `[]` |
| `initContainers` | Containers that run before Fluxer | `[]` |
