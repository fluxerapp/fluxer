// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ConfigObject, isConfigObject} from '@fluxer/config/src/config_loader/ConfigObject';

type ConfigPathKey = string | number;
type ConfigContainer = ConfigObject | Array<unknown>;

type EnvValueParser = (raw: string) => unknown;

interface NamedEnvOverride {
	path: Array<ConfigPathKey>;
	parse?: EnvValueParser;
}

const NAMED_FLUXER_ENV_OVERRIDES: Record<string, NamedEnvOverride> = {
	FLUXER_ENV: {path: ['env']},
	FLUXER_BASE_DOMAIN: {path: ['domain', 'base_domain']},
	FLUXER_PUBLIC_ORIGIN: {path: ['domain', 'public_origin']},
	FLUXER_PUBLIC_SCHEME: {path: ['domain', 'public_scheme']},
	FLUXER_PUBLIC_PORT: {path: ['domain', 'public_port'], parse: parseInteger},
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
	FLUXER_TRUST_CLIENT_IP_HEADER: {path: ['proxy', 'trust_client_ip_header'], parse: parseBoolean},
	FLUXER_CLIENT_IP_HEADER_NAME: {path: ['proxy', 'client_ip_header']},
	FLUXER_CASSANDRA_HOSTS: {path: ['database', 'cassandra', 'hosts'], parse: parseCsv},
	FLUXER_CASSANDRA_PORT: {path: ['database', 'cassandra', 'port'], parse: parseInteger},
	FLUXER_CASSANDRA_KEYSPACE: {path: ['database', 'cassandra', 'keyspace']},
	FLUXER_CASSANDRA_LOCAL_DC: {path: ['database', 'cassandra', 'local_dc']},
	FLUXER_CASSANDRA_USERNAME: {path: ['database', 'cassandra', 'username']},
	FLUXER_CASSANDRA_PASSWORD: {path: ['database', 'cassandra', 'password']},
	FLUXER_POSTGRES_URL: {path: ['database', 'postgres', 'url']},
	FLUXER_POSTGRES_HOST: {path: ['database', 'postgres', 'host']},
	FLUXER_POSTGRES_PORT: {path: ['database', 'postgres', 'port'], parse: parseInteger},
	FLUXER_POSTGRES_DATABASE: {path: ['database', 'postgres', 'database']},
	FLUXER_POSTGRES_USERNAME: {path: ['database', 'postgres', 'username']},
	FLUXER_POSTGRES_PASSWORD: {path: ['database', 'postgres', 'password']},
	FLUXER_POSTGRES_SSL: {path: ['database', 'postgres', 'ssl'], parse: parseBoolean},
	FLUXER_POSTGRES_SSL_CA: {path: ['database', 'postgres', 'ssl_ca']},
	FLUXER_POSTGRES_MAX_CONNECTIONS: {path: ['database', 'postgres', 'max_connections'], parse: parseInteger},
	FLUXER_POSTGRES_KV_TABLE: {path: ['database', 'postgres', 'kv_table']},
	FLUXER_POSTGRES_PREPARED_STATEMENTS: {path: ['database', 'postgres', 'prepared_statements'], parse: parseBoolean},
	FLUXER_DATABASE_BACKEND: {path: ['database', 'backend']},
	FLUXER_KV_URL: {path: ['internal', 'kv']},
	FLUXER_KV_MODE: {path: ['internal', 'kv_mode']},
	FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT: {path: ['internal', 'media_proxy']},
	FLUXER_S3_ENDPOINT: {path: ['s3', 'endpoint']},
	FLUXER_S3_PUBLIC_ENDPOINT: {path: ['s3', 'presigned_url_base']},
	FLUXER_S3_FORCE_PATH_STYLE: {path: ['s3', 'force_path_style'], parse: parseBoolean},
	FLUXER_S3_REGION: {path: ['s3', 'region']},
	FLUXER_S3_ACCESS_KEY_ID: {path: ['s3', 'access_key_id']},
	FLUXER_S3_SECRET_ACCESS_KEY: {path: ['s3', 'secret_access_key']},
	FLUXER_S3_BUCKET_CDN: {path: ['s3', 'buckets', 'cdn']},
	FLUXER_S3_BUCKET_UPLOADS: {path: ['s3', 'buckets', 'uploads']},
	FLUXER_S3_BUCKET_REPORTS: {path: ['s3', 'buckets', 'reports']},
	FLUXER_S3_BUCKET_HARVESTS: {path: ['s3', 'buckets', 'harvests']},
	FLUXER_NATS_URL: {path: ['services', 'nats', 'core_url']},
	FLUXER_NATS_JETSTREAM_URL: {path: ['services', 'nats', 'jetstream_url']},
	FLUXER_NATS_AUTH_TOKEN: {path: ['services', 'nats', 'auth_token']},
	FLUXER_API_PORT: {path: ['services', 'api', 'port'], parse: parseInteger},
	FLUXER_API_HEADERS_TIMEOUT_MS: {path: ['services', 'api', 'headers_timeout_ms'], parse: parseInteger},
	FLUXER_API_REQUEST_TIMEOUT_MS: {path: ['services', 'api', 'request_timeout_ms'], parse: parseInteger},
	FLUXER_API_MAX_INFLIGHT_REQUESTS: {path: ['services', 'api', 'max_inflight_requests'], parse: parseInteger},
	FLUXER_API_AUTOMATED_MESSAGE_DELETION_DELAY_DAYS: {
		path: ['services', 'api', 'automated_message_deletion_delay_days'],
		parse: parseInteger,
	},
	FLUXER_API_IP_BAN_EXEMPT_IPS: {path: ['services', 'api', 'ip_ban_exempt_ips'], parse: parseCsv},
	FLUXER_API_DONATION_PROXY_KEY: {path: ['services', 'api', 'donation_proxy_key']},
	FLUXER_API_TRUSTED_CALLERS: {path: ['services', 'api', 'trusted_callers'], parse: parseJsonArray},
	FLUXER_API_PRESIGNED_ATTACHMENT_UPLOADS_ENABLED: {
		path: ['services', 'api', 'presigned_attachment_uploads_enabled'],
		parse: parseBoolean,
	},
	FLUXER_API_PRESIGNED_HARVEST_DOWNLOADS_ENABLED: {
		path: ['services', 'api', 'presigned_harvest_downloads_enabled'],
		parse: parseBoolean,
	},
	FLUXER_API_WORKER_MODE: {path: ['services', 'api', 'worker', 'mode']},
	FLUXER_API_WORKER_LANE: {path: ['services', 'api', 'worker', 'lane']},
	FLUXER_API_WORKER_TASK: {path: ['services', 'api', 'worker', 'task']},
	FLUXER_API_WORKER_ENABLE_CRON_SCHEDULER: {
		path: ['services', 'api', 'worker', 'enable_cron_scheduler'],
		parse: parseBoolean,
	},
	FLUXER_API_WORKER_LANE_CONCURRENCY_OVERRIDES: {
		path: ['services', 'api', 'worker', 'lane_concurrency_overrides'],
		parse: parseJsonObject,
	},
	FLUXER_API_STORAGE_CHANGE_FEED_ENABLED: {
		path: ['services', 'api', 'storage_change_feed', 'enabled'],
		parse: parseBoolean,
	},
	FLUXER_API_STORAGE_CHANGE_FEED_STREAM: {path: ['services', 'api', 'storage_change_feed', 'stream']},
	FLUXER_API_STORAGE_CHANGE_FEED_SKIP_BUCKETS: {
		path: ['services', 'api', 'storage_change_feed', 'skip_buckets'],
		parse: parseBucketList,
	},
	FLUXER_API_UNFURL_IGNORED_HOSTS: {path: ['services', 'api', 'unfurl_ignored_hosts'], parse: parseCsv},
	FLUXER_APP_ORIGIN_ALIASES: {path: ['services', 'api', 'app_origin_aliases'], parse: parseCsv},
	FLUXER_MEDIA_PROXY_SECRET_KEY: {path: ['services', 'media_proxy', 'secret_key']},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT: {path: ['services', 'media_proxy', 'upload_relay', 'endpoint']},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: {path: ['services', 'media_proxy', 'upload_relay', 'secret_base64']},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES: {
		path: ['services', 'media_proxy', 'upload_relay', 'max_body_bytes'],
		parse: parseInteger,
	},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS: {
		path: ['services', 'media_proxy', 'upload_relay', 'token_ttl_secs'],
		parse: parseInteger,
	},
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_KEEP_DIRECT_COUNTRIES: {
		path: ['services', 'media_proxy', 'upload_relay', 'keep_direct_countries'],
		parse: parseCsv,
	},
	FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64: {
		path: ['services', 'media_proxy', 'attachment_urls', 'secrets_base64'],
		parse: parseCsv,
	},
	FLUXER_ADMIN_SECRET_KEY_BASE: {path: ['services', 'admin', 'secret_key_base']},
	FLUXER_ADMIN_OAUTH_CLIENT_SECRET: {path: ['services', 'admin', 'oauth_client_secret']},
	FLUXER_GATEWAY_RPC_AUTH_TOKEN: {path: ['services', 'gateway', 'rpc_auth_token']},
	FLUXER_SUDO_MODE_SECRET: {path: ['auth', 'sudo_mode_secret']},
	FLUXER_CONNECTION_INITIATION_SECRET: {path: ['auth', 'connection_initiation_secret']},
	FLUXER_PROFILE_PSEUDONYM_SECRET: {path: ['auth', 'profile_pseudonym_secret']},
	FLUXER_SSO_ALLOW_PRIVATE_ADDRESSES: {path: ['auth', 'sso_allow_private_addresses'], parse: parseBoolean},
	FLUXER_VAPID_PUBLIC_KEY: {path: ['auth', 'vapid', 'public_key']},
	FLUXER_VAPID_PRIVATE_KEY: {path: ['auth', 'vapid', 'private_key']},
	FLUXER_VAPID_EMAIL: {path: ['auth', 'vapid', 'email']},
	FLUXER_PASSKEY_RP_NAME: {path: ['auth', 'passkeys', 'rp_name']},
	FLUXER_PASSKEY_RP_ID: {path: ['auth', 'passkeys', 'rp_id']},
	FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: {
		path: ['auth', 'passkeys', 'additional_allowed_origins'],
		parse: parsePasskeyOrigins,
	},
	FLUXER_AUTH_BLUESKY_ENABLED: {path: ['auth', 'bluesky', 'enabled'], parse: parseBoolean},
	FLUXER_AUTH_BLUESKY_CLIENT_NAME: {path: ['auth', 'bluesky', 'client_name']},
	FLUXER_AUTH_BLUESKY_CLIENT_URI: {path: ['auth', 'bluesky', 'client_uri']},
	FLUXER_AUTH_BLUESKY_LOGO_URI: {path: ['auth', 'bluesky', 'logo_uri']},
	FLUXER_AUTH_BLUESKY_TOS_URI: {path: ['auth', 'bluesky', 'tos_uri']},
	FLUXER_AUTH_BLUESKY_POLICY_URI: {path: ['auth', 'bluesky', 'policy_uri']},
	FLUXER_AUTH_BLUESKY_KEYS: {path: ['auth', 'bluesky', 'keys'], parse: parseJsonArray},
	FLUXER_EMAIL_ENABLED: {path: ['integrations', 'email', 'enabled'], parse: parseBoolean},
	FLUXER_EMAIL_PROVIDER: {path: ['integrations', 'email', 'provider']},
	FLUXER_EMAIL_FROM_EMAIL: {path: ['integrations', 'email', 'from_email']},
	FLUXER_EMAIL_FROM_NAME: {path: ['integrations', 'email', 'from_name']},
	FLUXER_EMAIL_REPLY_TO_EMAIL: {path: ['integrations', 'email', 'reply_to_email']},
	FLUXER_EMAIL_APP_BASE_URL: {path: ['integrations', 'email', 'app_base_url']},
	FLUXER_EMAIL_WEBHOOK_SECRET: {path: ['integrations', 'email', 'webhook_secret']},
	FLUXER_EMAIL_SMTP_HOST: {path: ['integrations', 'email', 'smtp', 'host']},
	FLUXER_EMAIL_SMTP_PORT: {path: ['integrations', 'email', 'smtp', 'port'], parse: parseInteger},
	FLUXER_EMAIL_SMTP_USERNAME: {path: ['integrations', 'email', 'smtp', 'username']},
	FLUXER_EMAIL_SMTP_PASSWORD: {path: ['integrations', 'email', 'smtp', 'password']},
	FLUXER_EMAIL_SMTP_SECURE: {path: ['integrations', 'email', 'smtp', 'secure'], parse: parseBoolean},
	FLUXER_LIVEKIT_ENABLED: {path: ['integrations', 'voice', 'enabled'], parse: parseBoolean},
	FLUXER_LIVEKIT_API_KEY: {path: ['integrations', 'voice', 'api_key']},
	FLUXER_LIVEKIT_API_SECRET: {path: ['integrations', 'voice', 'api_secret']},
	FLUXER_LIVEKIT_URL: {path: ['integrations', 'voice', 'url']},
	FLUXER_LIVEKIT_INTERNAL_URL: {path: ['integrations', 'voice', 'internal_url']},
	FLUXER_LIVEKIT_DEFAULT_REGION: {path: ['integrations', 'voice', 'default_region'], parse: parseJsonObject},
	FLUXER_SEARCH_ENGINE: {path: ['integrations', 'search', 'engine']},
	FLUXER_SEARCH_URL: {path: ['integrations', 'search', 'url']},
	FLUXER_SEARCH_API_KEY: {path: ['integrations', 'search', 'api_key']},
	FLUXER_SEARCH_USERNAME: {path: ['integrations', 'search', 'username']},
	FLUXER_SEARCH_PASSWORD: {path: ['integrations', 'search', 'password']},
	FLUXER_SEARCH_TLS_REJECT_UNAUTHORIZED: {
		path: ['integrations', 'search', 'tls_reject_unauthorized'],
		parse: parseBoolean,
	},
	FLUXER_STRIPE_ENABLED: {path: ['integrations', 'stripe', 'enabled'], parse: parseBoolean},
	FLUXER_STRIPE_SECRET_KEY: {path: ['integrations', 'stripe', 'secret_key']},
	FLUXER_STRIPE_WEBHOOK_SECRET: {path: ['integrations', 'stripe', 'webhook_secret']},
	FLUXER_STRIPE_PRICES: {path: ['integrations', 'stripe', 'prices'], parse: parseJsonObject},
	FLUXER_STRIPE_LEGACY_PRICES: {path: ['integrations', 'stripe', 'legacy_prices'], parse: parseJsonObject},
	FLUXER_STRIPE_PRICE_MONTHLY_USD: {path: ['integrations', 'stripe', 'prices', 'monthly_usd']},
	FLUXER_STRIPE_PRICE_MONTHLY_EUR: {path: ['integrations', 'stripe', 'prices', 'monthly_eur']},
	FLUXER_STRIPE_PRICE_MONTHLY_BRL: {path: ['integrations', 'stripe', 'prices', 'monthly_brl']},
	FLUXER_STRIPE_PRICE_MONTHLY_DKK: {path: ['integrations', 'stripe', 'prices', 'monthly_dkk']},
	FLUXER_STRIPE_PRICE_MONTHLY_INR: {path: ['integrations', 'stripe', 'prices', 'monthly_inr']},
	FLUXER_STRIPE_PRICE_MONTHLY_NOK: {path: ['integrations', 'stripe', 'prices', 'monthly_nok']},
	FLUXER_STRIPE_PRICE_MONTHLY_PLN: {path: ['integrations', 'stripe', 'prices', 'monthly_pln']},
	FLUXER_STRIPE_PRICE_MONTHLY_SEK: {path: ['integrations', 'stripe', 'prices', 'monthly_sek']},
	FLUXER_STRIPE_PRICE_MONTHLY_TRY: {path: ['integrations', 'stripe', 'prices', 'monthly_try']},
	FLUXER_STRIPE_PRICE_YEARLY_USD: {path: ['integrations', 'stripe', 'prices', 'yearly_usd']},
	FLUXER_STRIPE_PRICE_YEARLY_EUR: {path: ['integrations', 'stripe', 'prices', 'yearly_eur']},
	FLUXER_STRIPE_PRICE_YEARLY_BRL: {path: ['integrations', 'stripe', 'prices', 'yearly_brl']},
	FLUXER_STRIPE_PRICE_YEARLY_DKK: {path: ['integrations', 'stripe', 'prices', 'yearly_dkk']},
	FLUXER_STRIPE_PRICE_YEARLY_INR: {path: ['integrations', 'stripe', 'prices', 'yearly_inr']},
	FLUXER_STRIPE_PRICE_YEARLY_NOK: {path: ['integrations', 'stripe', 'prices', 'yearly_nok']},
	FLUXER_STRIPE_PRICE_YEARLY_PLN: {path: ['integrations', 'stripe', 'prices', 'yearly_pln']},
	FLUXER_STRIPE_PRICE_YEARLY_SEK: {path: ['integrations', 'stripe', 'prices', 'yearly_sek']},
	FLUXER_STRIPE_PRICE_YEARLY_TRY: {path: ['integrations', 'stripe', 'prices', 'yearly_try']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_USD: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_usd']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_EUR: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_eur']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_SEK: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_sek']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_SEK: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_sek']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_DKK: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_dkk']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_DKK: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_dkk']},
	FLUXER_STRIPE_PRICE_GIFT_1_MONTH_NOK: {path: ['integrations', 'stripe', 'prices', 'gift_1_month_nok']},
	FLUXER_STRIPE_PRICE_GIFT_1_YEAR_NOK: {path: ['integrations', 'stripe', 'prices', 'gift_1_year_nok']},
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
	FLUXER_NCMEC_ENABLED: {path: ['integrations', 'ncmec', 'enabled'], parse: parseBoolean},
	FLUXER_NCMEC_BASE_URL: {path: ['integrations', 'ncmec', 'base_url']},
	FLUXER_NCMEC_USERNAME: {path: ['integrations', 'ncmec', 'username']},
	FLUXER_NCMEC_PASSWORD: {path: ['integrations', 'ncmec', 'password']},
	FLUXER_NCMEC_REPORTER_EMAIL: {path: ['integrations', 'ncmec', 'reporter_email']},
	FLUXER_CLAMAV_ENABLED: {path: ['integrations', 'clamav', 'enabled'], parse: parseBoolean},
	FLUXER_CLAMAV_HOST: {path: ['integrations', 'clamav', 'host']},
	FLUXER_CLAMAV_PORT: {path: ['integrations', 'clamav', 'port'], parse: parseInteger},
	FLUXER_CLAMAV_FAIL_OPEN: {path: ['integrations', 'clamav', 'fail_open'], parse: parseBoolean},
	FLUXER_KLIPY_API_KEY: {path: ['integrations', 'klipy', 'api_key']},
	FLUXER_YOUTUBE_API_KEY: {path: ['integrations', 'youtube', 'api_key']},
	FLUXER_CACHE_PURGE_ADAPTER: {path: ['integrations', 'cache_purge', 'adapter']},
	FLUXER_CACHE_PURGE_HTTP_ENDPOINT: {path: ['integrations', 'cache_purge', 'http', 'endpoint']},
	FLUXER_CACHE_PURGE_HTTP_TOKEN: {path: ['integrations', 'cache_purge', 'http', 'token']},
	FLUXER_CACHE_PURGE_HTTP_TIMEOUT_MS: {
		path: ['integrations', 'cache_purge', 'http', 'timeout_ms'],
		parse: parseInteger,
	},
	FLUXER_BLOCKLIST_FEEDS_ENABLED: {path: ['integrations', 'blocklist_feeds', 'enabled'], parse: parseBoolean},
	FLUXER_BREACHED_PASSWORD_CHECK_ENABLED: {
		path: ['integrations', 'breached_password_check', 'enabled'],
		parse: parseBoolean,
	},
	FLUXER_PUSH_APNS_ENABLED: {path: ['integrations', 'push', 'apns', 'enabled'], parse: parseBoolean},
	FLUXER_PUSH_APNS_TEAM_ID: {path: ['integrations', 'push', 'apns', 'team_id']},
	FLUXER_PUSH_APNS_KEY_ID: {path: ['integrations', 'push', 'apns', 'key_id']},
	FLUXER_PUSH_APNS_PRIVATE_KEY: {path: ['integrations', 'push', 'apns', 'private_key']},
	FLUXER_PUSH_APNS_PRIVATE_KEY_PATH: {path: ['integrations', 'push', 'apns', 'private_key_path']},
	FLUXER_PUSH_APNS_APPS: {path: ['integrations', 'push', 'apns', 'apps'], parse: parseJsonArray},
	FLUXER_APP_STORE_ENABLED: {path: ['integrations', 'app_store', 'enabled'], parse: parseBoolean},
	FLUXER_APP_STORE_ISSUER_ID: {path: ['integrations', 'app_store', 'issuer_id']},
	FLUXER_APP_STORE_KEY_ID: {path: ['integrations', 'app_store', 'key_id']},
	FLUXER_APP_STORE_PRIVATE_KEY: {path: ['integrations', 'app_store', 'private_key']},
	FLUXER_APP_STORE_PRIVATE_KEY_PATH: {path: ['integrations', 'app_store', 'private_key_path']},
	FLUXER_APP_STORE_APPS: {path: ['integrations', 'app_store', 'apps'], parse: parseJsonArray},
	FLUXER_APP_STORE_PRODUCTS: {path: ['integrations', 'app_store', 'products'], parse: parseJsonObject},
	FLUXER_GOOGLE_PLAY_ENABLED: {path: ['integrations', 'google_play', 'enabled'], parse: parseBoolean},
	FLUXER_GOOGLE_PLAY_PACKAGES: {path: ['integrations', 'google_play', 'packages'], parse: parseCsv},
	FLUXER_GOOGLE_PLAY_CLIENT_EMAIL: {path: ['integrations', 'google_play', 'client_email']},
	FLUXER_GOOGLE_PLAY_PRIVATE_KEY: {path: ['integrations', 'google_play', 'private_key']},
	FLUXER_GOOGLE_PLAY_PRIVATE_KEY_PATH: {path: ['integrations', 'google_play', 'private_key_path']},
	FLUXER_GOOGLE_PLAY_SERVICE_ACCOUNT_JSON_PATH: {path: ['integrations', 'google_play', 'service_account_json_path']},
	FLUXER_GOOGLE_PLAY_TOKEN_URI: {path: ['integrations', 'google_play', 'token_uri']},
	FLUXER_GOOGLE_PLAY_PRODUCTS: {path: ['integrations', 'google_play', 'products'], parse: parseJsonObject},
	FLUXER_GOOGLE_PLAY_PUSH_AUDIENCE: {path: ['integrations', 'google_play', 'push_audience']},
	FLUXER_GOOGLE_PLAY_PUSH_SERVICE_ACCOUNT_EMAIL: {
		path: ['integrations', 'google_play', 'push_service_account_email'],
	},
	FLUXER_STORE_BILLING_SANDBOX_USER_IDS: {path: ['integrations', 'store_billing', 'sandbox_user_ids'], parse: parseCsv},
	FLUXER_STORE_BILLING_SANDBOX_ENTITLES_ALL: {
		path: ['integrations', 'store_billing', 'sandbox_entitles_all'],
		parse: parseBoolean,
	},
	FLUXER_SELF_HOSTED: {path: ['instance', 'self_hosted'], parse: parseBoolean},
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
	FLUXER_APP_STATUS_PAGE_URL: {path: ['instance', 'branding', 'status_page_url']},
	FLUXER_APP_STATUS_PAGE_INCIDENT_HISTORY_URL: {path: ['instance', 'branding', 'status_page_incident_history_url']},
	FLUXER_INSTANCE_SETUP_CONFIGURED: {path: ['instance', 'setup', 'configured'], parse: parseBoolean},
	FLUXER_ACCOUNT_IDENTITY: {path: ['instance', 'account_identity'], parse: parseAccountIdentity},
	FLUXER_TAG_STYLE: {path: ['instance', 'tag_style'], parse: parseTagStyle},
	FLUXER_DISCOVERY_ENABLED: {path: ['discovery', 'enabled'], parse: parseBoolean},
	FLUXER_DISCOVERY_MIN_MEMBER_COUNT: {path: ['discovery', 'min_member_count'], parse: parseInteger},
	FLUXER_DELETION_GRACE_PERIOD_HOURS: {path: ['deletion_grace_period_hours'], parse: parseInteger},
	FLUXER_RELAX_REGISTRATION_RATE_LIMITS: {path: ['dev', 'relax_registration_rate_limits'], parse: parseBoolean},
	FLUXER_DISABLE_RATE_LIMITS: {path: ['dev', 'disable_rate_limits'], parse: parseBoolean},
	FLUXER_TEST_MODE_ENABLED: {path: ['dev', 'test_mode_enabled'], parse: parseBoolean},
	FLUXER_TEST_HARNESS_TOKEN: {path: ['dev', 'test_harness_token']},
	FLUXER_VALIDATE_RESPONSES: {path: ['dev', 'validate_responses'], parse: parseBoolean},
	FLUXER_GEOIP_DB_PATH: {path: ['geoip', 'maxmind_db_path']},
};

function isContainer(value: unknown): value is ConfigContainer {
	return isConfigObject(value) || Array.isArray(value);
}

function createChildContainer(nextKey: ConfigPathKey | undefined): ConfigContainer {
	return typeof nextKey === 'number' ? [] : {};
}

function getChildValue(target: ConfigContainer, key: ConfigPathKey): unknown {
	return Object.hasOwn(target, key) ? Reflect.get(target, key) : undefined;
}

function setChildValue(target: ConfigContainer, key: ConfigPathKey, value: unknown): void {
	if (key === '__proto__') {
		Object.defineProperty(target, key, {value, writable: true, enumerable: true, configurable: true});
		return;
	}
	if (Array.isArray(target) && typeof key === 'number') {
		target[key] = value;
		return;
	}
	(target as ConfigObject)[String(key)] = value;
}

function parseBoolean(raw: string): boolean {
	switch (raw.trim().toLowerCase()) {
		case 'true':
			return true;
		case 'false':
			return false;
		default:
			throw new Error('must be true or false');
	}
}

function parseAccountIdentity(raw: string): string {
	const value = raw.trim().toLowerCase();
	if (value === 'email' || value === 'username') return value;
	throw new Error('must be email or username');
}

function parseTagStyle(raw: string): string {
	const value = raw.trim().toLowerCase();
	if (value === 'none' || value === 'random') return value;
	throw new Error('must be none or random');
}

function parseJson(raw: string): unknown {
	try {
		return JSON.parse(raw);
	} catch {
		throw new Error('must be valid JSON');
	}
}

function parseJsonObject(raw: string): ConfigObject {
	const value = parseJson(raw);
	if (isConfigObject(value)) return value;
	throw new Error('must be a JSON object');
}

function parseJsonArray(raw: string): Array<unknown> {
	const value = parseJson(raw);
	if (Array.isArray(value)) return value;
	throw new Error('must be a JSON array');
}

function parseInteger(raw: string): number {
	const trimmed = raw.trim();
	if (!/^-?\d+$/.test(trimmed)) {
		throw new Error(`must be an integer, got ${JSON.stringify(raw)}`);
	}
	const value = Number(trimmed);
	if (!Number.isSafeInteger(value)) throw new Error('must be a safe integer');
	return value;
}

function parseCsv(raw: string): Array<string> {
	return raw
		.split(',')
		.map((part) => part.trim())
		.filter((part) => part.length > 0);
}

function parseBucketList(raw: string): Array<string> {
	return raw.trim().toLowerCase() === 'none' ? [] : parseCsv(raw);
}

function parsePasskeyOrigins(raw: string): Array<string> {
	if (/\p{Cc}/u.test(raw)) {
		throw new Error('must not contain control characters');
	}
	return parseCsv(raw);
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
	const current = getChildValue(target, first);
	let child: ConfigContainer;
	if (isContainer(current)) {
		child = current;
	} else {
		child = createChildContainer(rest[0]);
		setChildValue(target, first, child);
	}
	setNestedValue(child, rest, value);
}

const NAMED_FLUXER_ENV_ALIASES: Record<string, string | undefined> = {
	FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT: 'FLUXER_MEDIA_PROXY_ENDPOINT',
	FLUXER_NATS_URL: 'FLUXER_NATS_CORE_URL',
};

export const NAMED_FLUXER_ENV_NAMES = Object.keys(NAMED_FLUXER_ENV_OVERRIDES);

export function readEnvValue(env: NodeJS.ProcessEnv, name: string): string | undefined {
	const value = env[name];
	return value === undefined || value.trim().length === 0 ? undefined : value;
}

export function buildNamedFluxerEnvOverrides(env: NodeJS.ProcessEnv): ConfigObject {
	const overrides: ConfigObject = {};
	for (const [envKey, mapping] of Object.entries(NAMED_FLUXER_ENV_OVERRIDES)) {
		const alias = NAMED_FLUXER_ENV_ALIASES[envKey];
		const raw = readEnvValue(env, envKey) ?? (alias === undefined ? undefined : readEnvValue(env, alias));
		if (raw === undefined) {
			continue;
		}
		let parsed: unknown;
		try {
			parsed = mapping.parse ? mapping.parse(raw) : raw;
		} catch (error) {
			throw new Error(`${envKey} ${error instanceof Error ? error.message : String(error)}`);
		}
		setNestedValue(overrides, mapping.path, parsed);
	}
	return overrides;
}
