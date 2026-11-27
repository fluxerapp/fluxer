# SPDX-License-Identifier: AGPL-3.0-or-later

import json
import os
import re
import shutil
import subprocess
import sys
import tempfile

import yaml

CHART = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fluxer")
DOMAIN = "chat.fluxer.test"
KUBERNETES = ["1.29.0", "1.37.0"]
CRDS = "https://raw.githubusercontent.com/datreeio/CRDs-catalog/63669a570e231d4f1f8396d229a1de512bcf0a34/{{.Group}}/{{.ResourceKind}}_{{.ResourceAPIVersion}}.json"

EVERYTHING = {
    "domain": DOMAIN,
    "image": {"digest": "sha256:" + "0" * 64},
    "imagePullSecrets": [{"name": "registry"}],
    "fullnameOverride": "chat",
    "config": {"FLUXER_SEARCH_ENGINE": "none", "FLUXER_EMAIL_ENABLED": True, "FLUXER_EMAIL_SMTP_PORT": 587, "FLUXER_GIFS_SHARD_CACHE_MAX_BYTES": 134217728, "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES": 2147483648, "FLUXER_X_RATIO": 0.5},
    "existingSecret": "fluxer-mail",
    "postgres": {"externalSecret": "db-app"},
    "persistence": {"size": "50Gi", "storageClass": "fast", "annotations": {"k8up.io/backup": "true"}},
    "service": {"annotations": {"a": "b"}},
    "ingress": {"className": "traefik", "annotations": {"cert-manager.io/cluster-issuer": "letsencrypt", "nginx.ingress.kubernetes.io/proxy-read-timeout": 3600, "example.io/flag": True}},
    "httpRoute": {"enabled": True, "parentRefs": [{"name": "shared", "namespace": "gateway-system"}]},
    "voice": {"expose": "loadBalancer", "publicIP": "192.0.2.10", "service": {"annotations": {"a": "b"}}},
    "backup": {"schedule": "daily", "keep": 3, "hour": 4, "persistence": {"enabled": True, "storageClass": "nfs"}},
    "networkPolicy": {"enabled": True, "from": [{"namespaceSelector": {"matchLabels": {"kubernetes.io/metadata.name": "traefik"}}}]},
    "podAnnotations": {"a": "b"},
    "podLabels": {"team": "chat"},
    "priorityClassName": "high",
    "nodeSelector": {"kubernetes.io/arch": "arm64"},
    "tolerations": [{"key": "chat", "operator": "Exists"}],
    "affinity": {"nodeAffinity": {}},
    "extraEnv": [{"name": "NODE_EXTRA_CA_CERTS", "value": "/ca/ca.crt"}],
    "extraVolumes": [{"name": "ca", "configMap": {"name": "ca"}}],
    "extraVolumeMounts": [{"name": "ca", "mountPath": "/ca"}],
    "extraContainers": [{"name": "tunnel", "image": "tunnel:1"}],
    "initContainers": [{"name": "ca", "image": "ca:1"}],
}

SCENARIOS = {
    "default": {"domain": DOMAIN},
    "everything": EVERYTHING,
    "bundled": {"domain": DOMAIN, "edge": {"mode": "bundled", "loadBalancer": {"http3": True, "loadBalancerIP": "192.0.2.20"}}, "networkPolicy": {"enabled": True}},
    "nodeport": {"domain": DOMAIN, "image": {"tag": "2026.1114.195738"}, "voice": {"expose": "nodePort"}, "persistence": {"existingClaim": "old"}, "backup": {"persistence": {"enabled": True, "existingClaim": "nas"}}},
    "novoice": {"domain": DOMAIN, "voice": {"mode": "off"}, "ingress": {"enabled": False}, "config": {"FLUXER_UPGRADE_COMMAND": "git commit -am 'Fluxer {version}' && git push"}},
    "ipv6": {"domain": "2001:db8::7", "edge": {"mode": "bundled"}},
    "ipproxy": {"domain": "203.0.113.7", "ingress": {"enabled": False}},
    "ipdefault": {"domain": "192.0.2.10"},
    "hostport": {"domain": "192.168.1.5", "edge": {"expose": "hostPort"}},
    "voicelb": {"domain": DOMAIN, "voice": {"expose": "loadBalancer", "service": {"loadBalancerIP": "192.0.2.30"}}},
    "maintenance": {"domain": DOMAIN, "maintenance": True, "voice": {"mode": "external", "expose": "loadBalancer"}},
}

OLD_VALUES = {
    "domain": DOMAIN,
    "secrets": {"values": {"FLUXER_SUDO_MODE_SECRET": "x" * 32}},
    "postgres": {"enabled": True},
    "livekit": {"expose": "hostPort"},
    "api": {"replicas": 1},
    "edge": {"expose": "loadBalancer"},
}

REFUSED = [
    ("no domain", {}, "Set domain"),
    ("example domain", {"domain": "chat.example.com"}, "an example name"),
    ("domain with a scheme", {"domain": "https://chat.fluxer.test"}, ("at '/domain': 'anyOf' failed", "domain: Must validate at least one schema (anyOf)")),
    ("unknown key", {"domain": DOMAIN, "replicas": 2}, ("additional properties 'replicas' not allowed", "Additional property replicas is not allowed")),
    ("0.x values file", OLD_VALUES, ("additional properties", "Additional property")),
    ("0.x top-level values", {"domain": DOMAIN, "scheme": "https", "port": 443}, "port, scheme are values of the 0.x chart"),
    ("setting the chart owns", {"domain": DOMAIN, "config": {"FLUXER_EDGE_MODE": "bundled"}}, "Use edge.mode instead"),
    ("install method in config", {"domain": DOMAIN, "config": {"FLUXER_INSTALL_METHOD": "docker"}}, "Use config.FLUXER_UPGRADE_COMMAND instead"),
    ("password in config", {"domain": DOMAIN, "config": {"FLUXER_EMAIL_SMTP_PASSWORD": "hunter2"}}, "existingSecret"),
    ("lowercase config key", {"domain": DOMAIN, "config": {"fluxer_domain": "x"}}, ("does not match pattern '^[A-Z][A-Z0-9_]*$'", "Does not match pattern '^[A-Z][A-Z0-9_]*$'")),
    ("IP address behind an Ingress", {"domain": "203.0.113.7", "edge": {"mode": "proxy"}}, "only routes domain names"),
    ("IP address behind an HTTPRoute", {"domain": "2001:db8::7", "edge": {"mode": "proxy"}, "ingress": {"enabled": False}, "httpRoute": {"enabled": True, "parentRefs": [{"name": "shared"}]}}, "only routes domain names"),
    ("IPv6 address with two ::", {"domain": "2001:db8::7::1", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 address with three colons", {"domain": "2001:db8:::7", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 address with nine groups", {"domain": "1:2:3:4:5:6:7:8:9", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 address with seven groups and no ::", {"domain": "1:2:3:4:5:6:7", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 group of five digits", {"domain": "[2001:db8::12345]", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 address ending in a colon", {"domain": "2001:db8::7:", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 address with IPv4 in the middle", {"domain": "::192.0.2.1:7", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv6 address of colons only", {"domain": ":", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("IPv4 octet above 255", {"domain": "192.0.2.256", "edge": {"mode": "bundled"}}, "not a valid IPv4 or IPv6 address"),
    ("unspecified IPv4 address", {"domain": "0.0.0.0"}, "which no device can open"),
    ("unspecified IPv6 address", {"domain": "::"}, "which no device can open"),
    ("broadcast address", {"domain": "255.255.255.255"}, "which no device can open"),
    ("IPv4 multicast address", {"domain": "239.1.2.3"}, "which no device can open"),
    ("IPv6 multicast address", {"domain": "FF02::1"}, "which no device can open"),
    ("unknown edge expose", {"domain": DOMAIN, "edge": {"expose": "nodePort"}}, ("at '/edge/expose': value must be one of 'loadBalancer', 'hostPort'", 'edge.expose must be one of the following: "loadBalancer", "hostPort"')),
    ("host ports in proxy mode", {"domain": DOMAIN, "edge": {"expose": "hostPort"}}, "only opens ports 80 and 443 in bundled mode"),
    ("route without a gateway", {"domain": DOMAIN, "httpRoute": {"enabled": True}}, "httpRoute.parentRefs is empty"),
    ("unknown edge mode", {"domain": DOMAIN, "edge": {"mode": "tunnel"}}, ("at '/edge/mode': value must be one of '', 'proxy', 'bundled'", 'edge.mode must be one of the following: "", "proxy", "bundled"')),
    ("unknown voice mode", {"domain": DOMAIN, "voice": {"mode": "on"}}, ("at '/voice/mode': value must be one of 'bundled', 'external', 'off'", 'voice.mode must be one of the following: "bundled", "external", "off"')),
    ("node port out of range", {"domain": DOMAIN, "voice": {"nodePorts": {"tcp": 7881}}}, ("at '/voice/nodePorts/tcp': minimum: got 7,881, want 30,000", "voice.nodePorts.tcp: Must be greater than or equal to 30000")),
    ("bad backup schedule", {"domain": DOMAIN, "backup": {"schedule": "hourly"}}, ("at '/backup/schedule': value must be one of '', 'daily', 'weekly'", 'backup.schedule must be one of the following: "", "daily", "weekly"')),
]

VALID_IPS = ["192.0.2.10", "2001:DB8::7", "::1", "2001:db8::", "[2001:db8::7]", "1:2:3:4:5:6:7:8", "1:2:3:4:5:6:7::", "::2:3:4:5:6:7:8", "::ffff:192.0.2.7", "[64:ff9b::192.0.2.7]", "1:2:3:4:5:6:192.0.2.7", "fe80::1"]

failures = []


def fail(message):
    failures.append(message)
    print("FAIL " + message)


def values_file(tmp, name, values):
    path = os.path.join(tmp, name + ".yaml")
    with open(path, "w") as f:
        yaml.safe_dump(values, f)
    return path


def helm(command, *args, chart=CHART):
    name = [] if command == "lint" else ["fluxer"]
    return subprocess.run(["helm", command, *name, chart, *args], capture_output=True, text=True)


def render(tmp, name, values, chart=CHART):
    r = helm("template", "-n", "chat", "-f", values_file(tmp, name, values), "--api-versions", "gateway.networking.k8s.io/v1", chart=chart)
    if r.returncode != 0:
        fail(f"{name} does not render: {r.stderr.strip()}")
        return None, []
    return r.stdout, [d for d in yaml.safe_load_all(r.stdout) if d]


def kinds(docs):
    return {(d["kind"], d["metadata"]["name"]): d for d in docs}


def pod(docs):
    return next(d for d in docs if d["kind"] == "StatefulSet")["spec"]["template"]


def config(docs):
    return next(d for d in docs if d["kind"] == "ConfigMap")["data"]


def check_layout():
    files = [os.path.join(dp, f) for dp, _, fs in os.walk(CHART) for f in fs]
    if len(files) >= 15:
        fail(f"the chart has {len(files)} files, keep it under 15")
    with open(os.path.join(CHART, "README.md")) as f:
        readme = f.read()
    with open(os.path.join(CHART, "values.yaml")) as f:
        values = yaml.safe_load(f)

    def leaves(prefix, node):
        if isinstance(node, dict) and node and prefix != "resources":
            for k, v in node.items():
                yield from leaves(f"{prefix}.{k}" if prefix else k, v)
        else:
            yield prefix

    for key in leaves("", values):
        if f"`{key}`" not in readme:
            fail(f"README.md does not describe {key}")
    for path in files:
        if path.endswith((".yaml", ".tpl", ".txt")) and "templates" in path:
            with open(path) as f:
                if re.search(r"\{\{-?\s*/\*", f.read()):
                    fail(f"{path} has a template comment")


def check_scenarios(tmp):
    rendered = {}
    for name, values in SCENARIOS.items():
        r = helm("lint", "--strict", "-f", values_file(tmp, name, values))
        if r.returncode != 0:
            fail(f"helm lint {name}: {r.stdout}{r.stderr}")
        text, docs = render(tmp, name, values)
        if text is None:
            continue
        rendered[name] = docs
        with open(os.path.join(tmp, name + "-rendered.yaml"), "w") as f:
            f.write(text)
        for version in KUBERNETES:
            r = subprocess.run(["kubeconform", "-strict", "-summary", "-kubernetes-version", version, "-schema-location", "default", "-schema-location", CRDS, os.path.join(tmp, name + "-rendered.yaml")], capture_output=True, text=True)
            if r.returncode != 0:
                fail(f"kubeconform {name} on {version}: {r.stdout}{r.stderr}")
        if any(d["kind"] == "Secret" for d in docs):
            fail(f"{name} renders a Secret, the chart must create none")
        if "example.com" in text:
            fail(f"{name} renders example.com")
    return rendered


def check_render(rendered):
    d = rendered["default"]
    k = kinds(d)
    if ("Ingress", "fluxer") not in k or ("NetworkPolicy", "fluxer") in k or ("HTTPRoute", "fluxer") in k:
        fail("default should render an Ingress and no NetworkPolicy or HTTPRoute")
    spec = pod(d)["spec"]
    c = spec["containers"][0]
    if spec.get("enableServiceLinks") is not False or spec.get("automountServiceAccountToken") is not False:
        fail("the pod must turn off service links and the service account token")
    if "sysctls" in spec["securityContext"]:
        fail("proxy mode must not set the low port sysctl")
    probes = {p: c[p]["exec"]["command"][1:] for p in ("startupProbe", "livenessProbe", "readinessProbe")}
    if probes != {"startupProbe": ["health", "--live"], "livenessProbe": ["health", "--live"], "readinessProbe": ["health"]}:
        fail(f"probes are {probes}")
    hostports = sorted(p.get("hostPort") for p in c["ports"] if "hostPort" in p)
    if hostports != [7881, 7882]:
        fail(f"default voice host ports are {hostports}")
    upgrade = "helm upgrade fluxer oci://ghcr.io/fluxerapp/charts/fluxer -n chat --version {version} --reset-then-reuse-values"
    if config(d) != {"FLUXER_DOMAIN": DOMAIN, "FLUXER_EDGE_MODE": "proxy", "FLUXER_EDGE_TRUSTED_PROXIES": "private_ranges", "FLUXER_LIVEKIT_ENABLED": "true", "FLUXER_INSTALL_METHOD": "helm", "FLUXER_UPGRADE_COMMAND": upgrade}:
        fail(f"default config is {config(d)}")
    if d[0]["metadata"]["labels"]["app.kubernetes.io/version"] != "latest":
        fail("the image tag should default to appVersion")

    b = rendered["bundled"]
    spec = pod(b)["spec"]
    if spec["securityContext"].get("sysctls") != [{"name": "net.ipv4.ip_unprivileged_port_start", "value": "0"}]:
        fail("bundled mode must allow ports 80 and 443 through the sysctl")
    svc = kinds(b)[("Service", "fluxer")]["spec"]
    if svc["type"] != "LoadBalancer" or sorted((p["port"], p["protocol"]) for p in svc["ports"]) != [(80, "TCP"), (443, "TCP"), (443, "UDP")]:
        fail(f"bundled Service is {svc}")
    if any(x["kind"] == "Ingress" for x in b):
        fail("bundled mode renders an Ingress")
    if "FLUXER_EDGE_MODE" in config(b) or "FLUXER_EDGE_PROTOCOLS" in config(b):
        fail(f"bundled config is {config(b)}")

    i = rendered["ipv6"]
    if config(i).get("FLUXER_DOMAIN") != "2001:db8::7":
        fail("an IPv6 domain must reach Fluxer as set")
    for x in i:
        if x["kind"] == "Service" and x["spec"].get("ipFamilyPolicy") != "PreferDualStack":
            fail(f"an IPv6 domain must ask for dual-stack on {x['metadata']['name']}")
    if kinds(i)[("Service", "fluxer")]["spec"].get("loadBalancerIP") != "2001:db8::7":
        fail("an IPv6 domain must ask the load balancer for that address")
    if any(x["kind"] in ("Ingress", "HTTPRoute") for x in rendered["ipproxy"]):
        fail("an IP address in proxy mode renders an Ingress or HTTPRoute")
    if config(rendered["ipproxy"]).get("FLUXER_EDGE_MODE") != "proxy":
        fail("an IP address with the Ingress off must stay in proxy mode, as before edge.mode had an empty default")

    ip = rendered["ipdefault"]
    svc = kinds(ip)[("Service", "fluxer")]["spec"]
    if any(x["kind"] == "Ingress" for x in ip) or svc["type"] != "LoadBalancer" or svc.get("loadBalancerIP") != "192.0.2.10" or "ipFamilyPolicy" in svc:
        fail(f"an IPv4 domain must default to a bundled LoadBalancer on that address: {svc}")
    if "FLUXER_EDGE_MODE" in config(ip):
        fail(f"an IPv4 domain config is {config(ip)}")
    if kinds(rendered["bundled"])[("Service", "fluxer")]["spec"].get("loadBalancerIP") != "192.0.2.20":
        fail("an explicit loadBalancerIP must win")
    h = rendered["hostport"]
    hostports = sorted(p.get("hostPort") for p in pod(h)["spec"]["containers"][0]["ports"] if "hostPort" in p)
    if hostports != [80, 443, 7881, 7882] or kinds(h)[("Service", "fluxer")]["spec"]["type"] != "ClusterIP":
        fail(f"edge.expose hostPort must open 80 and 443 on the node with a ClusterIP Service, host ports are {hostports}")
    if config(ip).get("FLUXER_LIVEKIT_USE_EXTERNAL_IP") != "true" or "FLUXER_LIVEKIT_NODE_IP" in config(ip):
        fail(f"voice on the node must not advertise the edge load balancer's address: {config(ip)}")
    if "FLUXER_LIVEKIT_USE_EXTERNAL_IP" in config(h):
        fail(f"voice and the edge on the node's own address must keep using it: {config(h)}")
    if config(rendered["voicelb"]).get("FLUXER_LIVEKIT_NODE_IP") != "192.0.2.30":
        fail(f"voice on a load balancer must advertise its address: {config(rendered['voicelb'])}")

    e = rendered["everything"]
    k = kinds(e)
    for want in [("HTTPRoute", "chat"), ("NetworkPolicy", "chat"), ("Service", "chat-voice"), ("PersistentVolumeClaim", "chat-backups")]:
        if want not in k:
            fail(f"everything is missing {want}")
    c = pod(e)["spec"]["containers"][0]
    if any("hostPort" in p for p in c["ports"]):
        fail("voice on a load balancer must not use host ports")
    if c["image"] != "ghcr.io/fluxerapp/fluxer@sha256:" + "0" * 64:
        fail(f"the digest should win, image is {c['image']}")
    if config(e).get("FLUXER_UPGRADE_COMMAND") != "helm upgrade fluxer oci://ghcr.io/fluxerapp/charts/fluxer -n chat --version {version} --reset-then-reuse-values --set image.digest=":
        fail(f"with a digest the upgrade command must clear it: {config(e).get('FLUXER_UPGRADE_COMMAND')}")
    want = {"FLUXER_BACKUP_SCHEDULE": "daily", "FLUXER_BACKUP_KEEP": "3", "FLUXER_BACKUP_HOUR": "4", "FLUXER_LIVEKIT_NODE_IP": "192.0.2.10", "FLUXER_EMAIL_ENABLED": "true", "FLUXER_EMAIL_SMTP_PORT": "587",
            "FLUXER_GIFS_SHARD_CACHE_MAX_BYTES": "134217728", "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES": "2147483648", "FLUXER_X_RATIO": "0.5"}
    got = config(e)
    if any(got.get(key) != value for key, value in want.items()):
        fail(f"everything config is {got}")
    spec = pod(e)["spec"]
    if [x["name"] for x in spec["containers"]] != ["fluxer", "tunnel"] or [x["name"] for x in spec.get("initContainers", [])] != ["ca"]:
        fail("everything must add the extra and init containers")
    if k[("Ingress", "chat")]["metadata"].get("annotations") != {"cert-manager.io/cluster-issuer": "letsencrypt", "nginx.ingress.kubernetes.io/proxy-read-timeout": "3600", "example.io/flag": "true"}:
        fail(f"ingress.annotations must render numbers and booleans as strings: {k[('Ingress', 'chat')]['metadata'].get('annotations')}")
    if k[("Service", "chat")]["metadata"].get("annotations") != {"a": "b"}:
        fail("service.annotations must reach the proxy mode Service")
    claim = k[("StatefulSet", "chat")]["spec"]["volumeClaimTemplates"][0]["metadata"]
    if claim.get("annotations") != {"k8up.io/backup": "true"}:
        fail("persistence.annotations must reach the data claim")

    n = rendered["nodeport"]
    voice = kinds(n)[("Service", "fluxer-voice")]["spec"]
    if voice["type"] != "NodePort" or sorted(p["nodePort"] for p in voice["ports"]) != [30881, 30882]:
        fail(f"node port voice Service is {voice}")
    if config(n).get("FLUXER_UPGRADE_COMMAND") != "helm upgrade fluxer oci://ghcr.io/fluxerapp/charts/fluxer -n chat --version {version} --reset-then-reuse-values --set image.tag={version}":
        fail(f"with a tag the upgrade command must move it: {config(n).get('FLUXER_UPGRADE_COMMAND')}")
    if config(n).get("FLUXER_LIVEKIT_TCP_PORT") != "30881" or "volumeClaimTemplates" in kinds(n)[("StatefulSet", "fluxer")]["spec"]:
        fail("node port voice must advertise the node ports, and existingClaim must replace the claim template")

    v = rendered["novoice"]
    if any(x["kind"] == "Ingress" for x in v) or any(p["name"].startswith("voice") for p in pod(v)["spec"]["containers"][0]["ports"]):
        fail("voice off and ingress off still render voice ports or an Ingress")
    if config(v).get("FLUXER_UPGRADE_COMMAND") != "git commit -am 'Fluxer {version}' && git push":
        fail(f"config.FLUXER_UPGRADE_COMMAND must replace the chart's command: {config(v).get('FLUXER_UPGRADE_COMMAND')}")
    if config(v).get("FLUXER_LIVEKIT_ENABLED") != "false":
        fail(f"voice off config is {config(v)}")

    m = rendered["maintenance"]
    c = pod(m)["spec"]["containers"][0]
    if c.get("command") != ["/usr/bin/tini", "--", "sleep", "infinity"] or any(p in c for p in ("startupProbe", "livenessProbe", "readinessProbe")):
        fail("maintenance must run the pod without fluxerd and without probes")
    if any(p["name"].startswith("voice") for p in c["ports"]) or ("Service", "fluxer-voice") in kinds(m):
        fail("external voice must open no voice ports")
    if config(m).get("FLUXER_LIVEKIT_BUNDLED") != "false" or config(m).get("FLUXER_LIVEKIT_ENABLED") != "true":
        fail(f"external voice config is {config(m)}")


def check_refusals(tmp):
    for name, values, message in REFUSED:
        r = helm("template", "-f", values_file(tmp, "refused", values))
        if r.returncode == 0:
            fail(f"{name} renders")
        elif not any(m in r.stderr for m in ((message,) if isinstance(message, str) else message)):
            fail(f"{name} fails without '{message}': {r.stderr.strip()}")


def check_ip_domains(tmp):
    for domain in VALID_IPS:
        r = helm("template", "-f", values_file(tmp, "ip", {"domain": domain, "edge": {"mode": "bundled"}}))
        if r.returncode != 0:
            fail(f"the valid address {domain} is refused: {r.stderr.strip()}")


def notes(tmp, values):
    chart = os.path.join(tmp, "notes-chart")
    if not os.path.isdir(chart):
        shutil.copytree(CHART, chart)
        templates = os.path.join(chart, "templates")
        with open(os.path.join(templates, "NOTES.txt")) as f:
            body = f.read()
        os.remove(os.path.join(templates, "NOTES.txt"))
        with open(os.path.join(templates, "_notes-check.tpl"), "w") as f:
            f.write('{{- define "notes-check" -}}\n' + body + "{{- end -}}\n")
        with open(os.path.join(templates, "notes-check.yaml"), "w") as f:
            f.write('notes: {{ include "notes-check" . | toJson }}\n')
    r = helm("template", "-n", "chat", "--show-only", "templates/notes-check.yaml", "-f", values_file(tmp, "notes", values), chart=chart)
    if r.returncode != 0:
        fail(f"NOTES for {values} do not render: {r.stderr.strip()}")
        return ""
    return yaml.safe_load(r.stdout)["notes"]


def check_notes(tmp):
    for domain in ("2001:db8::7", "[2001:db8::7]", "192.168.1.5"):
        text = notes(tmp, {"domain": domain})
        if "ipFamil" in text or "fluxer-root.crt" not in text or "-fingerprint -sha256" not in text:
            fail(f"NOTES for {domain} mention IP families or leave out the root certificate and its fingerprint")


def check_version_bump(tmp):
    bumped = os.path.join(tmp, "bumped")
    shutil.copytree(CHART, bumped)
    with open(os.path.join(bumped, "Chart.yaml")) as f:
        chart = f.read()
    with open(os.path.join(bumped, "Chart.yaml"), "w") as f:
        f.write(re.sub(r"(?m)^version: .*$", "version: 99.0.0", chart))
    values = {"domain": DOMAIN, "image": {"tag": "2026.1201.120000"}}
    _, before = render(tmp, "before", values)
    _, after = render(tmp, "after", values, chart=bumped)
    if before and after and json.dumps(pod(before), sort_keys=True) != json.dumps(pod(after), sort_keys=True):
        fail("a chart version bump changes the pod template")


def main():
    for tool in ("helm", "kubeconform"):
        if not shutil.which(tool):
            sys.exit(f"{tool} is not on PATH")
    with tempfile.TemporaryDirectory() as tmp:
        check_layout()
        rendered = check_scenarios(tmp)
        if len(rendered) == len(SCENARIOS):
            check_render(rendered)
        check_refusals(tmp)
        check_ip_domains(tmp)
        check_notes(tmp)
        check_version_bump(tmp)
    if failures:
        sys.exit(f"{len(failures)} chart checks failed")
    print("chart checks passed")


main()
