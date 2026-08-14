# Get Started

Run your own Fluxer instance with Docker Compose. This guide takes you from a fresh server to a working self-hosted instance with the web app, API, gateway, admin dashboard, media uploads, search, storage, and voice signaling behind one public hostname.

## Requirements

- A Linux server or VM that can run Docker Engine.
- Docker Engine plus the Docker Compose plugin.
- A hostname for the instance, for example `chat.example.com`.
- Either public inbound `80/tcp` and `443/tcp`, or a Cloudflare Tunnel that routes the hostname to the Caddy container.
- For production voice and video media, a public path to `7881/tcp` and `7882/udp`.
- At least 2 vCPU, 4 GB RAM, and 20 GB disk. Use 4 vCPU and 8 GB RAM or more for a small active community.

The stack idles around a few GB of memory, and startup is the heaviest point because all service images initialize at once.

## Step 1: Install Docker

Install Docker Engine from Docker's official instructions for your distribution:

- [Install Docker Engine](https://docs.docker.com/engine/install/)
- [Install the Compose plugin](https://docs.docker.com/compose/install/linux/)
- [Linux post-installation steps](https://docs.docker.com/engine/install/linux-postinstall/)

Confirm the versions:

```bash
docker --version
docker compose version
```

Use Docker Engine 24 or newer and the Compose v2 plugin.

## Step 2: Download the stack

Create a working directory and download the stack files:

```bash
mkdir fluxer
cd fluxer

base=https://raw.githubusercontent.com/fluxerapp/fluxer/main/deploy/self-hosting
curl -fsSLO "$base/docker-compose.yml"
curl -fsSLO "$base/Caddyfile"
curl -fsSLO "$base/livekit.yaml"
curl -fsSL "$base/.env.example" -o .env
```

You should now have:

```text
Caddyfile
docker-compose.yml
livekit.yaml
.env
```

## Step 3: Configure `.env`

Set the public hostname at the top of `.env`.

For a normal public server where Caddy obtains certificates directly:

```ini
FLUXER_DOMAIN=chat.example.com
FLUXER_PUBLIC_SCHEME=https
FLUXER_PUBLIC_PORT=443
FLUXER_CADDY_SITE_ADDRESS=chat.example.com
FLUXER_VAPID_EMAIL=admin@example.com
```

For a Cloudflare Tunnel where Cloudflare terminates HTTPS and forwards HTTP to Caddy:

```ini
FLUXER_DOMAIN=chat.example.com
FLUXER_PUBLIC_SCHEME=https
FLUXER_PUBLIC_PORT=443
FLUXER_CADDY_SITE_ADDRESS=:80
FLUXER_VAPID_EMAIL=admin@example.com
```

`FLUXER_PUBLIC_SCHEME` and `FLUXER_PUBLIC_PORT` describe what users see in their browser. `FLUXER_CADDY_SITE_ADDRESS` describes what Caddy listens on inside the stack.

Generate the required secrets:

```bash
for key in POSTGRES_PASSWORD MEILI_MASTER_KEY FLUXER_S3_SECRET_KEY \
  FLUXER_SUDO_MODE_SECRET FLUXER_CONNECTION_INITIATION_SECRET \
  FLUXER_GATEWAY_RPC_AUTH_TOKEN FLUXER_MEDIA_PROXY_SECRET_KEY \
  FLUXER_ADMIN_SECRET_KEY_BASE FLUXER_ADMIN_OAUTH_CLIENT_SECRET \
  LIVEKIT_API_SECRET; do
  sed -i "s|^$key=.*|$key=$(openssl rand -hex 32)|" .env
done

sed -i "s|^FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64=.*|FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64=$(openssl rand -base64 32)|" .env

VAPID=$(docker run --rm node:24-alpine npx --yes web-push generate-vapid-keys --json)
pub=$(printf '%s' "$VAPID" | grep -o '"publicKey":"[^"]*"' | cut -d'"' -f4)
priv=$(printf '%s' "$VAPID" | grep -o '"privateKey":"[^"]*"' | cut -d'"' -f4)
sed -i "s|^FLUXER_VAPID_PUBLIC_KEY=.*|FLUXER_VAPID_PUBLIC_KEY=$pub|" .env
sed -i "s|^FLUXER_VAPID_PRIVATE_KEY=.*|FLUXER_VAPID_PRIVATE_KEY=$priv|" .env
```

Keep these defaults unless you know you need to change them:

- `LIVEKIT_API_KEY=fluxer`; the secret is `LIVEKIT_API_SECRET`.
- `FLUXER_S3_ACCESS_KEY=fluxer`; the secret is `FLUXER_S3_SECRET_KEY`.
- Email starts disabled. Enable SMTP later from `.env` and the admin dashboard.

!!! warning "Keep `.env` private"
    `.env` contains every secret for the instance. Do not commit it, paste it into support tickets, or put it in screenshots.

## Step 4: Publish the hostname

=== "Direct public server"

    Create DNS records for the hostname:

    - `A` record from `chat.example.com` to the server IPv4 address.
    - Optional `AAAA` record from `chat.example.com` to the server IPv6 address.

    Leave `FLUXER_CADDY_SITE_ADDRESS=chat.example.com`. Caddy will request and renew certificates automatically when `80/tcp` and `443/tcp` can reach the server.

=== "Cloudflare Tunnel"

    Use this when the server should not expose public web ports.

    1. Set `FLUXER_CADDY_SITE_ADDRESS=:80`.
    2. In Cloudflare, create a Tunnel public hostname for your Fluxer domain.
    3. If `cloudflared` runs inside the Compose project, point the public hostname service to `http://caddy:80`.
    4. If `cloudflared` runs directly on the host, point the public hostname service to `http://127.0.0.1:80`.

    A temporary Compose override keeps the tunnel next to Caddy without saving the token in your main stack:

    ```bash
    cat > cloudflared.compose.yml <<'YAML'
    services:
      cloudflared:
        image: cloudflare/cloudflared:latest
        restart: unless-stopped
        command: tunnel run --token ${CLOUDFLARED_TOKEN:?set CLOUDFLARED_TOKEN}
        depends_on:
          - caddy
        networks:
          - fluxer
    YAML

    export CLOUDFLARED_TOKEN='paste-your-tunnel-token-here'
    docker compose -f docker-compose.yml -f cloudflared.compose.yml up -d cloudflared
    ```

    !!! warning "Voice media is not carried by a normal public hostname tunnel"
        The web app, API, admin dashboard, gateway WebSocket, media proxy HTTP routes, and LiveKit signaling can work through the tunnel. LiveKit WebRTC media still needs reachable `7881/tcp` and `7882/udp`, or a TURN deployment.

## Step 5: Open the firewall

If you are using a direct public server, allow inbound:

- `22/tcp` or your SSH port.
- `80/tcp` and `443/tcp` for Caddy.
- `7881/tcp` and `7882/udp` for LiveKit media.

If you are using a Cloudflare Tunnel for web traffic, you can block inbound `80/tcp` and `443/tcp` at the provider firewall. Keep LiveKit media closed too unless you are intentionally exposing voice/video media or using a TURN server.

!!! warning "Provider firewall first"
    Docker-published ports can bypass host firewalls such as UFW because Docker installs its own packet-filtering rules. Prefer your cloud provider's firewall or security group for internet-facing policy.

## Step 6: Start the stack

Start Fluxer:

```bash
docker compose up -d
```

If you are using the Cloudflare override from above, start both files together:

```bash
docker compose -f docker-compose.yml -f cloudflared.compose.yml up -d
```

Watch the startup:

```bash
docker compose ps
docker compose logs -f api
```

The first start can take several minutes while images download and services initialize. `seaweedfs-init` exits after creating object-storage buckets; that is expected.

## Step 7: Create the owner account

Open the web app:

```text
https://chat.example.com
```

Register the first account. On a self-hosted instance, the first accepted registration receives wildcard admin access. Use that account for the initial admin login:

```text
https://chat.example.com/admin
```

Complete the initial setup from the admin dashboard. At minimum, review:

- Branding and instance name.
- Registration mode: open, approval, or closed.
- Email delivery.
- Captcha policy if you open public registration.
- Single-community mode if you want one default community instead of many user-created communities.
- Voice regions and LiveKit reachability if you are enabling voice.

## Email

Email is disabled by default. To enable SMTP, set these in `.env` and restart `api`, `worker`, and `admin`:

```ini
FLUXER_EMAIL_ENABLED=true
FLUXER_EMAIL_PROVIDER=smtp
FLUXER_EMAIL_FROM_EMAIL=noreply@example.com
FLUXER_EMAIL_FROM_NAME=Fluxer
FLUXER_EMAIL_SMTP_HOST=smtp.example.com
FLUXER_EMAIL_SMTP_PORT=587
FLUXER_EMAIL_SMTP_USERNAME=example
FLUXER_EMAIL_SMTP_PASSWORD=example-secret
FLUXER_EMAIL_SMTP_SECURE=true
```

Then test the SMTP configuration from `/admin/instance-config`.

## Voice and video

Fluxer uses LiveKit for voice and video. Caddy routes `/livekit` to LiveKit's HTTP/WebSocket signaling port, but browser media flows over WebRTC:

- `7882/udp` is the normal media path.
- `7881/tcp` is the TCP fallback path.
- `7880/tcp` stays private behind Caddy for signaling.

On a VPS with `7881/tcp` and `7882/udp` open, LiveKit can usually auto-detect the public IP. Behind NAT, Cloudflare Tunnel, or restrictive networks, add a TURN server and configure LiveKit for it.

## Backups

Back up these items before upgrades and on a regular schedule:

- `.env`
- `postgres-data`
- `seaweedfs-data`

For a cold backup:

```bash
docker compose stop api worker gateway admin app-proxy media-proxy static-proxy livekit
docker run --rm -v fluxer_postgres-data:/data -v "$PWD/backups:/backup" alpine tar czf /backup/postgres-data.tgz -C /data .
docker run --rm -v fluxer_seaweedfs-data:/data -v "$PWD/backups:/backup" alpine tar czf /backup/seaweedfs-data.tgz -C /data .
docker compose up -d
```

For production, prefer a Postgres-native dump plus object-storage backup so you do not need to stop the instance.

## Upgrading

The default image tag is `v1`, which tracks the latest compatible release:

```bash
docker compose pull
docker compose up -d
```

The `fluxer-static` image is part of the default stack, so static asset updates are picked up by the same pull-and-restart flow.

To pin a specific release, set `FLUXER_IMAGE_TAG` in `.env` to the release tag you want, then pull and restart.

## Getting help

- File issues and follow development on [GitHub](https://github.com/fluxerapp/fluxer).
- For direct access to the team, see [Operator Pass](operator-pass.md).
