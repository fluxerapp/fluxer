// SPDX-License-Identifier: AGPL-3.0-or-later

type ConfigPathKey = string | number;
type ConfigObject = Record<string, unknown>;
type ConfigContainer = ConfigObject | Array<unknown>;

type EnvValueParser = (raw: string) => unknown;

interface NamedEnvOverride {
	path: Array<ConfigPathKey>;
	parse?: EnvValueParser;
}

const NAMED_FLUXER_ENV_OVERRIDES: Record<string, NamedEnvOverride> = {
	FLUXER_ENV: {path: ['env']},
	FLUXER_BASE_DOMAIN: {path: ['domain', 'base_domain']},
	FLUXER_PUBLIC_SCHEME: {path: ['domain', 'public_scheme']},
	FLUXER_PUBLIC_PORT: {path: ['domain', 'public_port'], parse: parseEnvValue},
	FLUXER_STATIC_CDN_DOMAIN: {path: ['domain', 'static_cdn_domain']},
	FLUXER_INVITE_DOMAIN: {path: ['domain', 'invite_domain']},
	FLUXER_GIFT_DOMAIN: {path: ['domain', 'gift_domain']},
	FLUXER_API_ENDPOINT: {path: ['endpoint_overrides', 'api']},
	FLUXER_API_CLIENT_ENDPOINT: {path: ['endpoint_overrides', 'api_client']},
	FLUXER_APP_ENDPOINT: {path: ['endpoint_overrides', 'app']},
	FLUXER_GATEWAY_ENDPOINT: {path: ['endpoint_overrides', 'gateway']},
	FLUXER_MEDIA_ENDPOINT: {path: ['endpoint_overrides', 'media']},
	FLUXER_STATIC_CDN_ENDPOINT: {path: ['endpoint_overrides', 'static_cdn']},
	FLUXER_ADMIN_ENDPOINT: {path: ['endpoint_overrides', 'admin']},
	FLUXER_MARKETING_ENDPOINT: {path: ['endpoint_overrides', 'marketing']},
	FLUXER_INVITE_ENDPOINT: {path: ['endpoint_overrides', 'invite']},
	FLUXER_GIFT_ENDPOINT: {path: ['endpoint_overrides', 'gift']},
	FLUXER_TRUST_CLIENT_IP_HEADER: {path: ['proxy', 'trust_client_ip_header'], parse: parseEnvValue},
	FLUXER_CLIENT_IP_HEADER_NAME: {path: ['proxy', 'client_ip_header']},
	FLUXER_CASSANDRA_HOSTS: {path: ['database', 'cassandra', 'hosts'], parse: parseCsv},
	FLUXER_CASSANDRA_PORT: {path: ['database', 'cassandra', 'port'], parse: parseEnvValue},
	FLUXER_CASSANDRA_KEYSPACE: {path: ['database', 'cassandra', 'keyspace']},
	FLUXER_CASSANDRA_LOCAL_DC: {path: ['database', 'cassandra', 'local_dc']},
	FLUXER_CASSANDRA_USERNAME: {path: ['database', 'cassandra', 'username']},
	FLUXER_CASSANDRA_PASSWORD: {path: ['database', 'cassandra', 'password']},
	FLUXER_POSTGRES_URL: {path: ['database', 'postgres', 'url']},
	FLUXER_POSTGRES_HOST: {path: ['database', 'postgres', 'host']},
	FLUXER_POSTGRES_PORT: {path: ['database', 'postgres', 'port'], parse: parseEnvValue},
	FLUXER_POSTGRES_DATABASE: {path: ['database', 'postgres', 'database']},
	FLUXER_POSTGRES_USERNAME: {path: ['database', 'postgres', 'username']},
	FLUXER_POSTGRES_PASSWORD: {path: ['database', 'postgres', 'password']},
	FLUXER_POSTGRES_SSL: {path: ['database', 'postgres', 'ssl'], parse: parseEnvValue},
	FLUXER_POSTGRES_SSL_CA: {path: ['database', 'postgres', 'ssl_ca']},
	FLUXER_POSTGRES_MAX_CONNECTIONS: {path: ['database', 'postgres', 'max_connections'], parse: parseEnvValue},
	FLUXER_POSTGRES_KV_TABLE: {path: ['database', 'postgres', 'kv_table']},
	FLUXER_DATABASE_BACKEND: {path: ['database', 'backend']},
	FLUXER_KV_URL: {path: ['internal', 'kv']},
	FLUXER_INTERNAL_API_ENDPOINT: {path: ['internal', 'api']},
	FLUXER_INTERNAL_GATEWAY_ENDPOINT: {path: ['internal', 'gateway']},
	FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT: {path: ['internal', 'media_proxy']},
	FLUXER_MEDIA_PROXY_ENDPOINT: {path: ['internal', 'media_proxy']},
	FLUXER_S3_ENDPOINT: {path: ['s3', 'endpoint']},
	FLUXER_S3_PUBLIC_ENDPOINT: {path: ['s3', 'presigned_url_base']},
	FLUXER_S3_FORCE_PATH_STYLE: {path: ['s3', 'force_path_style'], parse: parseEnvValue},
	FLUXER_S3_REGION: {path: ['s3', 'region']},
	FLUXER_S3_ACCESS_KEY_ID: {path: ['s3', 'access_key_id']},
	FLUXER_S3_SECRET_ACCESS_KEY: {path: ['s3', 'secret_access_key']},
	FLUXER_S3_BUCKET_CDN: {path: ['s3', 'buckets', 'cdn']},
	FLUXER_S3_BUCKET_UPLOADS: {path: ['s3', 'buckets', 'uploads']},
	FLUXER_S3_BUCKET_DOWNLOADS: {path: ['s3', 'buckets', 'downloads']},
	FLUXER_S3_BUCKET_REPORTS: {path: ['s3', 'buckets', 'reports']},
	FLUXER_S3_BUCKET_HARVESTS: {path: ['s3', 'buckets', 'harvests']},
	FLUXER_S3_BUCKET_STATIC: {path: ['s3', 'buckets', 'static']},
	FLUXER_NATS_URL: {path: ['services', 'nats', 'core_url']},
	FLUXER_NATS_CORE_URL: {path: ['services', 'nats', 'core_url']},
	FLUXER_NATS_JETSTREAM_URL: {path: ['services', 'nats', 'jetstream_url']},
	FLUXER_NATS_AUTH_TOKEN: {path: ['services', 'nats', 'auth_token']},
	FLUXER_API_PORT: {path: ['services', 'api', 'port'], parse: parseEnvValue},
	FLUXER_API_PRESIGNED_ATTACHMENT_UPLOADS_ENABLED: {
		path: ['services', 'api', 'presigned_attachment_uploads_enabled'],
		parse: parseEnvValue,
	},
	FLUXER_API_WORKER_MODE: {path: ['services', 'api', 'worker', 'mode']},
	FLUXER_API_WORKER_LANE: {path: ['services', 'api', 'worker', 'lane']},
	FLUXER_API_WORKER_TASK: {path: ['services', 'api', 'worker', 'task']},
	FLUXER_API_WORKER_ENABLE_CRON_SCHEDULER: {
		path: ['services', 'api', 'worker', 'enable_cron_scheduler'],
		parse: parseEnvValue,
	},
	FLUXER_API_WORKER_ENABLE_VOICE_RECONCILIATION: {
		path: ['services', 'api', 'worker', 'enable_voice_reconciliation'],
		parse: parseEnvValue,
	},
	FLUXER_API_WORKER_LANE_CONCURRENCY_OVERRIDES: {
		path: ['services', 'api', 'worker', 'lane_concurrency_overrides'],
		parse: parseEnvValue,
	},
	FLUXER_API_UNFURL_IGNORED_HOSTS: {path: ['services', 'api', 'unfurl_ignored_hosts'], parse: parseCsv},
	FLUXER_API_EMBEDS_OEMBED_HTML_ENABLED: {
		path: ['services', 'api', 'embeds', 'oembed_html_enabled'],
		parse: parseEnvValue,
	},
	FLUXER_API_EMBEDS_OEMBED_HTML_ALLOW_UNTRUSTED_ON_SELF_HOSTED: {
		path: ['services', 'api', 'embeds', 'oembed_html_allow_untrusted_on_self_hosted'],
		parse: parseEnvValue,
	},
	FLUXER_API_EMBEDS_OEMBED_HTML_ALLOWED_HOSTS: {
		path: ['services', 'api', 'embeds', 'oembed_html_allowed_hosts'],
		parse: parseCsv,
	},
	FLUXER_API_EMBEDS_CACHE_DEFAULT_TTL_SECONDS: {
		path: ['services', 'api', 'embeds', 'cache_default_ttl_seconds'],
		parse: parseEnvValue,
	},
	FLUXER_API_EMBEDS_CACHE_MAX_TTL_SECONDS: {
		path: ['services', 'api', 'embeds', 'cache_max_ttl_seconds'],
		parse: parseEnvValue,
	},
	FLUXER_API_EMBEDS_CACHE_MIN_TTL_SECONDS: {
		path: ['services', 'api', 'embeds', 'cache_min_ttl_seconds'],
		parse: parseEnvValue,
	},
	FLUXER_API_EMBEDS_CACHE_RESPECT_REMOTE_TTL: {
		path: ['services', 'api', 'embeds', 'cache_respect_remote_ttl'],
		parse: parseEnvValue,
	},
	FLUXER_API_CONTENT_MODERATION_NSFW_THRESHOLD: {
		path: ['services', 'api', 'content_moderation', 'nsfw_threshold'],
		parse: parseEnvValue,
	},
	FLUXER_MEDIA_PROXY_HOST: {path: ['services', 'media_proxy', 'host']},
	FLUXER_MEDIA_PROXY_PORT: {path: ['services', 'media_proxy', 'port'], parse: parseEnvValue},
	FLUXER_MEDIA_PROXY_SECRET_KEY: {path: ['services', 'media_proxy', 'secret_key']},
	FLUXER_MEDIA_PROXY_MODE: {path: ['services', 'media_proxy', 'mode']},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT: {path: ['services', 'media_proxy', 'upload_relay', 'endpoint']},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES: {
		path: ['services', 'media_proxy', 'upload_relay', 'max_body_bytes'],
		parse: parseEnvValue,
	},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS: {
		path: ['services', 'media_proxy', 'upload_relay', 'token_ttl_secs'],
		parse: parseEnvValue,
	},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_KEEP_DIRECT_COUNTRIES: {
		path: ['services', 'media_proxy', 'upload_relay', 'keep_direct_countries'],
		parse: parseCsv,
	},
	FLUXER_ADMIN_PORT: {path: ['services', 'admin', 'port'], parse: parseEnvValue},
	FLUXER_ADMIN_BASE_PATH: {path: ['services', 'admin', 'base_path']},
	FLUXER_ADMIN_SECRET_KEY_BASE: {path: ['services', 'admin', 'secret_key_base']},
	FLUXER_ADMIN_OAUTH_CLIENT_SECRET: {path: ['services', 'admin', 'oauth_client_secret']},
	FLUXER_MARKETING_HOST: {path: ['services', 'marketing', 'host']},
	FLUXER_MARKETING_PORT: {path: ['services', 'marketing', 'port'], parse: parseEnvValue},
	FLUXER_MARKETING_BASE_PATH: {path: ['services', 'marketing', 'base_path']},
	FLUXER_MARKETING_SECRET_KEY_BASE: {path: ['services', 'marketing', 'secret_key_base']},
	FLUXER_APP_PROXY_PORT: {path: ['services', 'app_proxy', 'port'], parse: parseEnvValue},
	FLUXER_STATIC_DIR: {path: ['services', 'app_proxy', 'assets_dir']},
	FLUXER_GATEWAY_PORT: {path: ['services', 'gateway', 'port'], parse: parseEnvValue},
	FLUXER_GATEWAY_ROLE: {path: ['services', 'gateway', 'gateway_role']},
	FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT: {path: ['services', 'gateway', 'media_proxy_endpoint']},
	FLUXER_GATEWAY_API_RPC_ENDPOINT: {path: ['services', 'gateway', 'api_rpc_endpoint']},
	FLUXER_GATEWAY_RPC_AUTH_TOKEN: {path: ['services', 'gateway', 'rpc_auth_token']},
	FLUXER_GATEWAY_PUSH_ENABLED: {path: ['services', 'gateway', 'push_enabled'], parse: parseEnvValue},
	FLUXER_GATEWAY_LOGGER_LEVEL: {path: ['services', 'gateway', 'logger_level']},
	FLUXER_GATEWAY_HTTP_FAILURE_THRESHOLD: {
		path: ['services', 'gateway', 'gateway_http_failure_threshold'],
		parse: parseEnvValue,
	},
	FLUXER_GATEWAY_HTTP_RECOVERY_TIMEOUT_MS: {
		path: ['services', 'gateway', 'gateway_http_recovery_timeout_ms'],
		parse: parseEnvValue,
	},
	FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY: {
		path: ['services', 'gateway', 'gateway_http_rpc_max_concurrency'],
		parse: parseEnvValue,
	},
	FLUXER_GATEWAY_SHUTDOWN_DRAIN_WAIT_MS: {
		path: ['services', 'gateway', 'shutdown_drain_wait_ms'],
		parse: parseEnvValue,
	},
	FLUXER_GATEWAY_CLUSTER_ENABLED: {path: ['services', 'gateway', 'cluster_enabled'], parse: parseEnvValue},
	FLUXER_GATEWAY_CLUSTER_DISCOVERY_DNS_NAME: {path: ['services', 'gateway', 'cluster_discovery_dns_name']},
	FLUXER_GATEWAY_CLUSTER_DISCOVERY_NODE_BASENAME: {
		path: ['services', 'gateway', 'cluster_discovery_node_basename'],
	},
	FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS: {
		path: ['services', 'gateway', 'cluster_discovery_poll_interval_ms'],
		parse: parseEnvValue,
	},
	FLUXER_SUDO_MODE_SECRET: {path: ['auth', 'sudo_mode_secret']},
	FLUXER_CONNECTION_INITIATION_SECRET: {path: ['auth', 'connection_initiation_secret']},
	FLUXER_VAPID_PUBLIC_KEY: {path: ['auth', 'vapid', 'public_key']},
	FLUXER_VAPID_PRIVATE_KEY: {path: ['auth', 'vapid', 'private_key']},
	FLUXER_VAPID_EMAIL: {path: ['auth', 'vapid', 'email']},
	FLUXER_PASSKEY_RP_NAME: {path: ['auth', 'passkeys', 'rp_name']},
	FLUXER_PASSKEY_RP_ID: {path: ['auth', 'passkeys', 'rp_id']},
	FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: {
		path: ['auth', 'passkeys', 'additional_allowed_origins'],
		parse: parseCsv,
	},
	FLUXER_AUTH_BLUESKY_ENABLED: {path: ['auth', 'bluesky', 'enabled'], parse: parseEnvValue},
	FLUXER_AUTH_BLUESKY_CLIENT_NAME: {path: ['auth', 'bluesky', 'client_name']},
	FLUXER_AUTH_BLUESKY_CLIENT_URI: {path: ['auth', 'bluesky', 'client_uri']},
	FLUXER_AUTH_BLUESKY_LOGO_URI: {path: ['auth', 'bluesky', 'logo_uri']},
	FLUXER_AUTH_BLUESKY_TOS_URI: {path: ['auth', 'bluesky', 'tos_uri']},
	FLUXER_AUTH_BLUESKY_POLICY_URI: {path: ['auth', 'bluesky', 'policy_uri']},
	FLUXER_AUTH_BLUESKY_KEYS: {path: ['auth', 'bluesky', 'keys'], parse: parseEnvValue},
	FLUXER_EMAIL_ENABLED: {path: ['integrations', 'email', 'enabled'], parse: parseEnvValue},
	FLUXER_EMAIL_PROVIDER: {path: ['integrations', 'email', 'provider']},
	FLUXER_EMAIL_FROM_EMAIL: {path: ['integrations', 'email', 'from_email']},
	FLUXER_EMAIL_FROM_NAME: {path: ['integrations', 'email', 'from_name']},
	FLUXER_EMAIL_WEBHOOK_SECRET: {path: ['integrations', 'email', 'webhook_secret']},
	FLUXER_EMAIL_SMTP_HOST: {path: ['integrations', 'email', 'smtp', 'host']},
	FLUXER_EMAIL_SMTP_PORT: {path: ['integrations', 'email', 'smtp', 'port'], parse: parseEnvValue},
	FLUXER_EMAIL_SMTP_USERNAME: {path: ['integrations', 'email', 'smtp', 'username']},
	FLUXER_EMAIL_SMTP_PASSWORD: {path: ['integrations', 'email', 'smtp', 'password']},
	FLUXER_EMAIL_SMTP_SECURE: {path: ['integrations', 'email', 'smtp', 'secure'], parse: parseEnvValue},
	FLUXER_SMS_ENABLED: {path: ['integrations', 'sms', 'enabled'], parse: parseEnvValue},
	FLUXER_SMS_ACCOUNT_SID: {path: ['integrations', 'sms', 'account_sid']},
	FLUXER_SMS_AUTH_TOKEN: {path: ['integrations', 'sms', 'auth_token']},
	FLUXER_SMS_VERIFY_SERVICE_SID: {path: ['integrations', 'sms', 'verify_service_sid']},
	FLUXER_SMS_INBOUND_CHALLENGE_NUMBER: {path: ['integrations', 'sms', 'inbound_challenge_number']},
	FLUXER_SMS_INBOUND_WEBHOOK_AUTH_TOKEN: {path: ['integrations', 'sms', 'inbound_webhook_auth_token']},
	FLUXER_SMS_INBOUND_WEBHOOK_PUBLIC_URL: {path: ['integrations', 'sms', 'inbound_webhook_public_url']},
	FLUXER_CAPTCHA_ENABLED: {path: ['integrations', 'captcha', 'enabled'], parse: parseEnvValue},
	FLUXER_CAPTCHA_PROVIDER: {path: ['integrations', 'captcha', 'provider']},
	FLUXER_CAPTCHA_HCAPTCHA_SITE_KEY: {path: ['integrations', 'captcha', 'hcaptcha', 'site_key']},
	FLUXER_CAPTCHA_HCAPTCHA_SECRET_KEY: {path: ['integrations', 'captcha', 'hcaptcha', 'secret_key']},
	FLUXER_CAPTCHA_TURNSTILE_SITE_KEY: {path: ['integrations', 'captcha', 'turnstile', 'site_key']},
	FLUXER_CAPTCHA_TURNSTILE_SECRET_KEY: {path: ['integrations', 'captcha', 'turnstile', 'secret_key']},
	FLUXER_LIVEKIT_ENABLED: {path: ['integrations', 'voice', 'enabled'], parse: parseEnvValue},
	FLUXER_LIVEKIT_API_KEY: {path: ['integrations', 'voice', 'api_key']},
	FLUXER_LIVEKIT_API_SECRET: {path: ['integrations', 'voice', 'api_secret']},
	FLUXER_LIVEKIT_URL: {path: ['integrations', 'voice', 'url']},
	FLUXER_LIVEKIT_WEBHOOK_URL: {path: ['integrations', 'voice', 'webhook_url']},
	FLUXER_LIVEKIT_DEFAULT_REGION: {path: ['integrations', 'voice', 'default_region'], parse: parseEnvValue},
	FLUXER_SEARCH_ENGINE: {path: ['integrations', 'search', 'engine']},
	FLUXER_SEARCH_URL: {path: ['integrations', 'search', 'url']},
	FLUXER_SEARCH_API_KEY: {path: ['integrations', 'search', 'api_key']},
	FLUXER_SEARCH_USERNAME: {path: ['integrations', 'search', 'username']},
	FLUXER_SEARCH_PASSWORD: {path: ['integrations', 'search', 'password']},
	FLUXER_SEARCH_TLS_REJECT_UNAUTHORIZED: {
		path: ['integrations', 'search', 'tls_reject_unauthorized'],
		parse: parseEnvValue,
	},
	FLUXER_STRIPE_ENABLED: {path: ['integrations', 'stripe', 'enabled'], parse: parseEnvValue},
	FLUXER_STRIPE_SECRET_KEY: {path: ['integrations', 'stripe', 'secret_key']},
	FLUXER_STRIPE_WEBHOOK_SECRET: {path: ['integrations', 'stripe', 'webhook_secret']},
	FLUXER_STRIPE_PRICES: {path: ['integrations', 'stripe', 'prices'], parse: parseEnvValue},
	FLUXER_STRIPE_PRICE_MONTHLY_USD: {path: ['integrations', 'stripe', 'prices', 'monthly_usd']},
	FLUXER_STRIPE_PRICE_MONTHLY_EUR: {path: ['integrations', 'stripe', 'prices', 'monthly_eur']},
	FLUXER_STRIPE_PRICE_MONTHLY_BRL: {path: ['integrations', 'stripe', 'prices', 'monthly_brl']},
	FLUXER_STRIPE_PRICE_MONTHLY_INR: {path: ['integrations', 'stripe', 'prices', 'monthly_inr']},
	FLUXER_STRIPE_PRICE_MONTHLY_PLN: {path: ['integrations', 'stripe', 'prices', 'monthly_pln']},
	FLUXER_STRIPE_PRICE_MONTHLY_TRY: {path: ['integrations', 'stripe', 'prices', 'monthly_try']},
	FLUXER_STRIPE_PRICE_YEARLY_USD: {path: ['integrations', 'stripe', 'prices', 'yearly_usd']},
	FLUXER_STRIPE_PRICE_YEARLY_EUR: {path: ['integrations', 'stripe', 'prices', 'yearly_eur']},
	FLUXER_STRIPE_PRICE_YEARLY_BRL: {path: ['integrations', 'stripe', 'prices', 'yearly_brl']},
	FLUXER_STRIPE_PRICE_YEARLY_INR: {path: ['integrations', 'stripe', 'prices', 'yearly_inr']},
	FLUXER_STRIPE_PRICE_YEARLY_PLN: {path: ['integrations', 'stripe', 'prices', 'yearly_pln']},
	FLUXER_STRIPE_PRICE_YEARLY_TRY: {path: ['integrations', 'stripe', 'prices', 'yearly_try']},
	FLUXER_STRIPE_PRICE_VISIONARY_USD: {path: ['integrations', 'stripe', 'prices', 'visionary_usd']},
	FLUXER_STRIPE_PRICE_VISIONARY_EUR: {path: ['integrations', 'stripe', 'prices', 'visionary_eur']},
	FLUXER_STRIPE_PRICE_GIFT_VISIONARY_USD: {path: ['integrations', 'stripe', 'prices', 'gift_visionary_usd']},
	FLUXER_STRIPE_PRICE_GIFT_VISIONARY_EUR: {path: ['integrations', 'stripe', 'prices', 'gift_visionary_eur']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_USD: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_usd']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_EUR: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_eur']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_BRL: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_brl']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_INR: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_inr']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_PLN: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_pln']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_TRY: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_try']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_USD: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_usd']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_EUR: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_eur']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_BRL: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_brl']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_INR: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_inr']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_PLN: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_pln']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_TRY: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_try']},
	FLUXER_NCMEC_ENABLED: {path: ['integrations', 'ncmec', 'enabled'], parse: parseEnvValue},
	FLUXER_NCMEC_BASE_URL: {path: ['integrations', 'ncmec', 'base_url']},
	FLUXER_NCMEC_USERNAME: {path: ['integrations', 'ncmec', 'username']},
	FLUXER_NCMEC_PASSWORD: {path: ['integrations', 'ncmec', 'password']},
	FLUXER_NCMEC_REPORTER_EMAIL: {path: ['integrations', 'ncmec', 'reporter_email']},
	FLUXER_CLAMAV_ENABLED: {path: ['integrations', 'clamav', 'enabled'], parse: parseEnvValue},
	FLUXER_CLAMAV_HOST: {path: ['integrations', 'clamav', 'host']},
	FLUXER_CLAMAV_PORT: {path: ['integrations', 'clamav', 'port'], parse: parseEnvValue},
	FLUXER_CLAMAV_FAIL_OPEN: {path: ['integrations', 'clamav', 'fail_open'], parse: parseEnvValue},
	FLUXER_KLIPY_API_KEY: {path: ['integrations', 'klipy', 'api_key']},
	FLUXER_YOUTUBE_API_KEY: {path: ['integrations', 'youtube', 'api_key']},
	FLUXER_BUNNY_PURGE_ENABLED: {path: ['integrations', 'bunny', 'purge_enabled'], parse: parseEnvValue},
	FLUXER_BUNNY_API_KEY: {path: ['integrations', 'bunny', 'api_key']},
	FLUXER_BUNNY_PULL_ZONE_ID: {path: ['integrations', 'bunny', 'pull_zone_id'], parse: parseEnvValue},
	FLUXER_RISK_INTEGRATION_ENABLED: {path: ['integrations', 'risk_integration', 'enabled'], parse: parseEnvValue},
	FLUXER_RISK_IPINFO_API_KEY: {path: ['integrations', 'risk_integration', 'ipinfo_api_key']},
	FLUXER_ACCOUNT_POLICY_DSL: {
		path: ['integrations', 'risk_integration', 'account_policy_dsl'],
		parse: parseEnvValue,
	},
	FLUXER_RISK_TOR_BLOCK_ALL_RELAYS: {
		path: ['integrations', 'risk_integration', 'tor', 'block_all_relays'],
		parse: parseEnvValue,
	},
	FLUXER_RISK_TOR_REVERSE_DNS_HEURISTIC: {
		path: ['integrations', 'risk_integration', 'tor', 'reverse_dns_heuristic'],
		parse: parseEnvValue,
	},
	FLUXER_RISK_TOR_REVERSE_DNS_TIMEOUT_MS: {
		path: ['integrations', 'risk_integration', 'tor', 'reverse_dns_timeout_ms'],
		parse: parseEnvValue,
	},
	FLUXER_PUSH_APNS_ENABLED: {path: ['integrations', 'push', 'apns', 'enabled'], parse: parseEnvValue},
	FLUXER_PUSH_APNS_TEAM_ID: {path: ['integrations', 'push', 'apns', 'team_id']},
	FLUXER_PUSH_APNS_KEY_ID: {path: ['integrations', 'push', 'apns', 'key_id']},
	FLUXER_PUSH_APNS_PRIVATE_KEY: {path: ['integrations', 'push', 'apns', 'private_key']},
	FLUXER_PUSH_APNS_PRIVATE_KEY_PATH: {path: ['integrations', 'push', 'apns', 'private_key_path']},
	FLUXER_PUSH_APNS_DEFAULT_ENVIRONMENT: {path: ['integrations', 'push', 'apns', 'default_environment']},
	FLUXER_PUSH_APNS_APPS: {path: ['integrations', 'push', 'apns', 'apps'], parse: parseEnvValue},
	FLUXER_PUSH_FCM_ENABLED: {path: ['integrations', 'push', 'fcm', 'enabled'], parse: parseEnvValue},
	FLUXER_PUSH_FCM_PROJECT_ID: {path: ['integrations', 'push', 'fcm', 'project_id']},
	FLUXER_PUSH_FCM_CLIENT_EMAIL: {path: ['integrations', 'push', 'fcm', 'client_email']},
	FLUXER_PUSH_FCM_PRIVATE_KEY: {path: ['integrations', 'push', 'fcm', 'private_key']},
	FLUXER_PUSH_FCM_PRIVATE_KEY_PATH: {path: ['integrations', 'push', 'fcm', 'private_key_path']},
	FLUXER_PUSH_FCM_SERVICE_ACCOUNT_JSON_PATH: {path: ['integrations', 'push', 'fcm', 'service_account_json_path']},
	FLUXER_PUSH_FCM_TOKEN_URI: {path: ['integrations', 'push', 'fcm', 'token_uri']},
	FLUXER_PUSH_FCM_APPS: {path: ['integrations', 'push', 'fcm', 'apps'], parse: parseEnvValue},
	FLUXER_COOKIE_DOMAIN: {path: ['cookie', 'domain']},
	FLUXER_COOKIE_SECURE: {path: ['cookie', 'secure'], parse: parseEnvValue},
	FLUXER_SELF_HOSTED: {path: ['instance', 'self_hosted'], parse: parseEnvValue},
	FLUXER_AUTO_JOIN_INVITE_CODE: {path: ['instance', 'auto_join_invite_code']},
	FLUXER_VISIONARIES_GUILD_ID: {path: ['instance', 'visionaries_guild_id']},
	FLUXER_VISIONARIES_GUILD_VISIONARY_ROLE_ID: {path: ['instance', 'visionaries_guild_visionary_role_id']},
	FLUXER_APP_PRODUCT_NAME: {path: ['instance', 'branding', 'product_name']},
	FLUXER_APP_ICON_URL: {path: ['instance', 'branding', 'icon_url']},
	FLUXER_APP_SYMBOL_URL: {path: ['instance', 'branding', 'symbol_url']},
	FLUXER_APP_LOGO_URL: {path: ['instance', 'branding', 'logo_url']},
	FLUXER_APP_WORDMARK_URL: {path: ['instance', 'branding', 'wordmark_url']},
	FLUXER_APP_FAVICON_URL: {path: ['instance', 'branding', 'favicon_url']},
	FLUXER_APP_THEME_COLOR: {path: ['instance', 'branding', 'theme_color']},
	FLUXER_INSTANCE_SETUP_CONFIGURED: {path: ['instance', 'setup', 'configured'], parse: parseEnvValue},
	FLUXER_ABUSE_INBOUND_PHONE_COUNTRY_CODES: {
		path: ['instance', 'abuse_policy', 'inbound_phone_country_codes'],
		parse: parseCsv,
	},
	FLUXER_ABUSE_PHONE_INBOUND_REQUIRED_PREFIXES: {
		path: ['instance', 'abuse_policy', 'phone_verification', 'inbound_required_prefixes'],
		parse: parseCsv,
	},
	FLUXER_ABUSE_DIRECT_CONTACT_SPAM_ENABLED: {
		path: ['instance', 'abuse_policy', 'direct_contact_spam', 'enabled'],
		parse: parseEnvValue,
	},
	FLUXER_ABUSE_DIRECT_CONTACT_SPAM_COUNTRY_CODES: {
		path: ['instance', 'abuse_policy', 'direct_contact_spam', 'country_codes'],
		parse: parseCsv,
	},
	FLUXER_ABUSE_DIRECT_CONTACT_SPAM_DISTINCT_TARGET_THRESHOLD: {
		path: ['instance', 'abuse_policy', 'direct_contact_spam', 'distinct_target_threshold'],
		parse: parseEnvValue,
	},
	FLUXER_ABUSE_DIRECT_CONTACT_SPAM_TARGET_WINDOW_MS: {
		path: ['instance', 'abuse_policy', 'direct_contact_spam', 'target_window_ms'],
		parse: parseEnvValue,
	},
	FLUXER_ABUSE_DIRECT_CONTACT_SPAM_ACTION: {
		path: ['instance', 'abuse_policy', 'direct_contact_spam', 'action'],
	},
	FLUXER_DISCOVERY_ENABLED: {path: ['discovery', 'enabled'], parse: parseEnvValue},
	FLUXER_DISCOVERY_MIN_MEMBER_COUNT: {path: ['discovery', 'min_member_count'], parse: parseEnvValue},
	FLUXER_RELAX_REGISTRATION_RATE_LIMITS: {path: ['dev', 'relax_registration_rate_limits'], parse: parseEnvValue},
	FLUXER_DISABLE_RATE_LIMITS: {path: ['dev', 'disable_rate_limits'], parse: parseEnvValue},
	FLUXER_TEST_MODE_ENABLED: {path: ['dev', 'test_mode_enabled'], parse: parseEnvValue},
	FLUXER_TEST_HARNESS_TOKEN: {path: ['dev', 'test_harness_token']},
	FLUXER_GEOIP_DB_PATH: {path: ['geoip', 'maxmind_db_path']},
};

function isPlainObject(value: unknown): value is ConfigObject {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function isContainer(value: unknown): value is ConfigContainer {
	return isPlainObject(value) || Array.isArray(value);
}

function createChildContainer(nextKey: ConfigPathKey | undefined): ConfigContainer {
	return typeof nextKey === 'number' ? [] : {};
}

function getChildValue(target: ConfigContainer, key: ConfigPathKey): unknown {
	return target[key as keyof typeof target];
}

function setChildValue(target: ConfigContainer, key: ConfigPathKey, value: unknown): void {
	if (Array.isArray(target) && typeof key === 'number') {
		target[key] = value;
		return;
	}
	(target as ConfigObject)[String(key)] = value;
}

function toChildContainer(value: unknown, nextKey: ConfigPathKey | undefined): ConfigContainer {
	if (isContainer(value)) {
		return value;
	}
	return createChildContainer(nextKey);
}

export function parseEnvValue(raw: string): unknown {
	const trimmed = raw.trim();
	const lower = trimmed.toLowerCase();
	if (lower === 'true') {
		return true;
	}
	if (lower === 'false') {
		return false;
	}
	if (/^-?\d+$/.test(trimmed)) {
		return Number.parseInt(trimmed, 10);
	}
	if (/^-?\d+\.\d+$/.test(trimmed)) {
		return Number.parseFloat(trimmed);
	}
	if ((trimmed.startsWith('{') && trimmed.endsWith('}')) || (trimmed.startsWith('[') && trimmed.endsWith(']'))) {
		try {
			return JSON.parse(trimmed);
		} catch {
			return raw;
		}
	}
	return raw;
}

function parseCsv(raw: string): Array<string> {
	return raw
		.split(',')
		.map((part) => part.trim())
		.filter((part) => part.length > 0);
}

export function setNestedValue(target: ConfigContainer, keys: Array<ConfigPathKey>, value: unknown): void {
	if (keys.length === 0) {
		return;
	}
	const [first, ...rest] = keys;
	if (rest.length === 0) {
		setChildValue(target, first, value);
		return;
	}
	const child = getChildValue(target, first);
	if (!isContainer(child)) {
		setChildValue(target, first, createChildContainer(rest[0]));
	}
	setNestedValue(toChildContainer(getChildValue(target, first), rest[0]), rest, value);
}

export function buildNamedFluxerEnvOverrides(env: NodeJS.ProcessEnv): ConfigObject {
	const overrides: ConfigObject = {};
	for (const [envKey, mapping] of Object.entries(NAMED_FLUXER_ENV_OVERRIDES)) {
		const raw = env[envKey];
		if (raw === undefined) {
			continue;
		}
		setNestedValue(overrides, mapping.path, (mapping.parse ?? ((value: string) => value))(raw));
	}
	return overrides;
}
