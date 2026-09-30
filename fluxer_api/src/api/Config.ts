// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig, BlueskyOAuthConfig} from '@app/api/config/APIConfig';
import {parseIpBanEntry} from '@app/api/utils/IpRangeUtils';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {parseGeoipSourceConfig, resolveGeoipRuntimeSourceConfig} from '@pkgs/geoip/src/GeoipStartup';

function extractHostname(url: string): string {
	try {
		return new URL(url).hostname;
	} catch {
		throw new Error(`Invalid URL: ${url}`);
	}
}

function trimTrailingSlash(url: string): string {
	return url.replace(/\/+$/u, '');
}

function resolveEmailAppBaseUrl(master: MasterConfig): string {
	const configuredAppBaseUrl = master.integrations.email.app_base_url.trim();
	if (!configuredAppBaseUrl) return trimTrailingSlash(master.endpoints.app);
	try {
		const appBaseUrl = new URL(configuredAppBaseUrl);
		if (
			(appBaseUrl.protocol !== 'http:' && appBaseUrl.protocol !== 'https:') ||
			appBaseUrl.username ||
			appBaseUrl.password ||
			appBaseUrl.search ||
			appBaseUrl.hash
		) {
			throw new Error(`Invalid email app base URL: ${configuredAppBaseUrl}`);
		}
		return trimTrailingSlash(appBaseUrl.toString());
	} catch {
		throw new Error(`Invalid email app base URL: ${configuredAppBaseUrl}`);
	}
}

function isBoolean(value: unknown): value is boolean {
	return typeof value === 'boolean';
}

function resolveValidateResponses(master: MasterConfig): boolean {
	if (isBoolean(master.dev.validate_responses)) {
		return master.dev.validate_responses;
	}
	return master.env !== 'production';
}

function resolveTrustClientIpHeader(proxyConfig: object): boolean {
	const configuredValue = Reflect.get(proxyConfig, 'trust_client_ip_header');
	if (isBoolean(configuredValue)) {
		return configuredValue;
	}
	return false;
}

function normalizeIpBanExemptIps(values: Array<string>): Array<string> {
	const normalized = new Set<string>();
	for (const value of values) {
		if (value.includes('/')) {
			const range = parseIpBanEntry(value);
			if (range?.type !== 'range') {
				throw new Error(`FLUXER_API_IP_BAN_EXEMPT_IPS contains an invalid CIDR range: ${value}`);
			}
			normalized.add(range.canonical);
			continue;
		}
		const parsed = parseIpAddress(value);
		if (!parsed) {
			throw new Error(`FLUXER_API_IP_BAN_EXEMPT_IPS contains an invalid IP address: ${value}`);
		}
		normalized.add(parsed.normalized);
	}
	return Array.from(normalized);
}

function mapApnsApps(
	apps:
		| Array<{
				app_id?: string;
				topic?: string;
				environment?: 'production' | 'development';
		  }>
		| undefined,
): APIConfig['push']['apns']['apps'] {
	return (apps ?? []).map((app) => {
		if (!app.app_id) {
			throw new Error('FLUXER_PUSH_APNS_APPS contains an entry with no app_id');
		}
		return {
			appId: app.app_id,
			topic: app.topic,
			environment: app.environment,
		};
	});
}

export function buildAPIConfigFromMaster(master: MasterConfig): APIConfig {
	if (!master.internal) {
		throw new Error('internal configuration is required for the API');
	}
	const cassandraSource = master.database.cassandra;
	const postgresSource = master.database.postgres;
	const apiWorkerConfig = master.services.api?.worker;
	const s3Config = master.s3;
	const geoipSourceConfig = resolveGeoipRuntimeSourceConfig(parseGeoipSourceConfig(master.geoip.maxmind_db_path), {
		serviceName: 'api',
	});
	const uploadRelayConfig = master.services.media_proxy.upload_relay;
	const uploadRelaySecretBase64 = uploadRelayConfig.secret_base64;
	if (uploadRelaySecretBase64.length === 0) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required for the API');
	}
	if (Buffer.from(uploadRelaySecretBase64, 'base64').length < 32) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must decode to at least 32 bytes');
	}
	const donationProxyKey = (master.services.api.donation_proxy_key ?? '').trim();
	if (donationProxyKey.length > 0 && donationProxyKey.length < 32) {
		throw new Error('FLUXER_API_DONATION_PROXY_KEY must be at least 32 characters');
	}
	if (!s3Config) {
		throw new Error('S3 configuration is required for the API');
	}
	const s3Buckets = s3Config.buckets ?? {
		cdn: '',
		uploads: '',
		reports: '',
		harvests: '',
	};
	if (master.database.backend === 'cassandra' && !cassandraSource) {
		throw new Error('Cassandra configuration is required.');
	}
	if (master.database.backend === 'postgres' && !postgresSource) {
		throw new Error('Postgres configuration is required.');
	}
	return {
		nodeEnv: master.env === 'test' ? 'development' : master.env,
		port: master.services.api.port,
		headersTimeoutMs: master.services.api.headers_timeout_ms,
		requestTimeoutMs: master.services.api.request_timeout_ms,
		maxInflightRequests: master.services.api.max_inflight_requests,
		ipBanExemptIps: normalizeIpBanExemptIps(master.services.api.ip_ban_exempt_ips),
		cassandra: {
			hosts: cassandraSource?.hosts.join(',') ?? '',
			port: cassandraSource?.port ?? 9042,
			keyspace: cassandraSource?.keyspace ?? '',
			localDc: cassandraSource?.local_dc ?? '',
			username: cassandraSource?.username ?? '',
			password: cassandraSource?.password ?? '',
		},
		postgres: {
			url: postgresSource?.url ?? '',
			host: postgresSource?.host ?? '127.0.0.1',
			port: postgresSource?.port ?? 5432,
			database: postgresSource?.database ?? 'fluxer',
			username: postgresSource?.username ?? 'fluxer',
			password: postgresSource?.password ?? 'fluxer',
			ssl: postgresSource?.ssl ?? false,
			sslCa: postgresSource?.ssl_ca ?? '',
			maxConnections: postgresSource?.max_connections ?? 20,
			kvTable: postgresSource?.kv_table ?? 'fluxer_kv',
			preparedStatements: postgresSource?.prepared_statements ?? true,
		},
		database: {
			backend: master.database.backend,
		},
		kv: {
			url: master.internal.kv,
			mode: master.internal.kv_mode,
		},
		nats: {
			coreUrl: master.services.nats?.core_url ?? 'nats://127.0.0.1:4222',
			jetStreamUrl: master.services.nats?.jetstream_url ?? 'nats://127.0.0.1:4223',
			authToken: master.services.nats?.auth_token ?? '',
		},
		storageChangeFeed: {
			enabled: master.services.api.storage_change_feed?.enabled ?? false,
			stream: master.services.api.storage_change_feed?.stream ?? 'STORAGE_CHANGES',
			skipBuckets: master.services.api.storage_change_feed?.skip_buckets ?? [s3Buckets.uploads],
		},
		search: {
			engine: master.integrations.search?.engine ?? 'elasticsearch',
			url: master.integrations.search?.url ?? 'http://127.0.0.1:9200',
			apiKey: master.integrations.search?.api_key ?? '',
			username: master.integrations.search?.username ?? '',
			password: master.integrations.search?.password ?? '',
			tlsRejectUnauthorized: master.integrations.search?.tls_reject_unauthorized ?? true,
		},
		mediaProxy: {
			host: extractHostname(master.internal.media_proxy),
			port: new URL(master.internal.media_proxy).port
				? Number.parseInt(new URL(master.internal.media_proxy).port, 10)
				: 80,
			secretKey: master.services.media_proxy.secret_key,
			uploadRelay: {
				endpoint: trimTrailingSlash(uploadRelayConfig.endpoint),
				relaySecretBase64: uploadRelaySecretBase64,
				maxBodyBytes: uploadRelayConfig.max_body_bytes,
				tokenTtlSecs: uploadRelayConfig.token_ttl_secs,
				keepDirectCountries: uploadRelayConfig.keep_direct_countries,
			},
			attachmentUrls: {
				secretsBase64: master.services.media_proxy.attachment_urls.secrets_base64,
			},
		},
		geoip: geoipSourceConfig,
		proxy: {
			trust_client_ip_header: resolveTrustClientIpHeader(master.proxy),
			client_ip_header: (Reflect.get(master.proxy, 'client_ip_header') as string | undefined) ?? 'x-forwarded-for',
		},
		endpoints: {
			apiPublic: master.endpoints.api,
			apiClient: master.endpoints.api_client,
			webApp: master.endpoints.app,
			webAppOrigins: [...new Set([new URL(master.endpoints.app).origin, ...master.services.api.app_origin_aliases])],
			gateway: master.endpoints.gateway,
			media: master.endpoints.media,
			marketing: master.endpoints.marketing,
			admin: master.endpoints.admin,
			invite: master.endpoints.invite,
			gift: master.endpoints.gift,
			staticCdn: master.endpoints.static_cdn,
		},
		internal: {
			gatewayRpcAuthToken: master.services.gateway.rpc_auth_token ?? '',
			donationProxyKey,
		},
		hosts: {
			marketing: extractHostname(master.endpoints.marketing),
			unfurlIgnored: master.services.api.unfurl_ignored_hosts,
		},
		s3: {
			endpoint: s3Config.endpoint,
			presignedUrlBase: s3Config.presigned_url_base,
			forcePathStyle: s3Config.force_path_style,
			region: s3Config.region,
			accessKeyId: s3Config.access_key_id,
			secretAccessKey: s3Config.secret_access_key,
			buckets: s3Buckets,
		},
		email: {
			enabled: master.integrations.email.enabled,
			provider: master.integrations.email.provider,
			webhookSecret: master.integrations.email.webhook_secret ?? undefined,
			fromEmail: master.integrations.email.from_email,
			fromName: master.integrations.email.from_name,
			appBaseUrl: resolveEmailAppBaseUrl(master),
			smtp: master.integrations.email.smtp
				? {
						host: master.integrations.email.smtp.host,
						port: master.integrations.email.smtp.port,
						username: master.integrations.email.smtp.username,
						password: master.integrations.email.smtp.password,
						secure: master.integrations.email.smtp.secure ?? true,
					}
				: undefined,
		},
		blocklistFeeds: {
			enabled: master.integrations.blocklist_feeds.enabled ?? !master.instance.self_hosted,
		},
		breachedPasswordCheck: {
			enabled: master.integrations.breached_password_check.enabled ?? !master.instance.self_hosted,
		},
		voice: {
			enabled: master.integrations.voice.enabled,
			apiKey: master.integrations.voice.api_key,
			apiSecret: master.integrations.voice.api_secret,
			url: master.integrations.voice.url,
			internalUrl: master.integrations.voice.internal_url,
			defaultRegion: master.integrations.voice.default_region,
		},
		stripe: {
			enabled: master.integrations.stripe.enabled,
			secretKey: master.integrations.stripe.secret_key,
			webhookSecret: master.integrations.stripe.webhook_secret,
			prices: master.integrations.stripe.prices
				? {
						monthlyUsd: master.integrations.stripe.prices.monthly_usd,
						monthlyEur: master.integrations.stripe.prices.monthly_eur,
						monthlyBrl: master.integrations.stripe.prices.monthly_brl,
						monthlyDkk: master.integrations.stripe.prices.monthly_dkk,
						monthlyInr: master.integrations.stripe.prices.monthly_inr,
						monthlyNok: master.integrations.stripe.prices.monthly_nok,
						monthlyPln: master.integrations.stripe.prices.monthly_pln,
						monthlySek: master.integrations.stripe.prices.monthly_sek,
						monthlyTry: master.integrations.stripe.prices.monthly_try,
						yearlyUsd: master.integrations.stripe.prices.yearly_usd,
						yearlyEur: master.integrations.stripe.prices.yearly_eur,
						yearlyBrl: master.integrations.stripe.prices.yearly_brl,
						yearlyDkk: master.integrations.stripe.prices.yearly_dkk,
						yearlyInr: master.integrations.stripe.prices.yearly_inr,
						yearlyNok: master.integrations.stripe.prices.yearly_nok,
						yearlyPln: master.integrations.stripe.prices.yearly_pln,
						yearlySek: master.integrations.stripe.prices.yearly_sek,
						yearlyTry: master.integrations.stripe.prices.yearly_try,
						gift1MonthUsd: master.integrations.stripe.prices.gift_1_month_usd,
						gift1MonthEur: master.integrations.stripe.prices.gift_1_month_eur,
						gift1MonthSek: master.integrations.stripe.prices.gift_1_month_sek,
						gift1YearSek: master.integrations.stripe.prices.gift_1_year_sek,
						gift1MonthDkk: master.integrations.stripe.prices.gift_1_month_dkk,
						gift1YearDkk: master.integrations.stripe.prices.gift_1_year_dkk,
						gift1MonthNok: master.integrations.stripe.prices.gift_1_month_nok,
						gift1YearNok: master.integrations.stripe.prices.gift_1_year_nok,
						gift1MonthBrl: master.integrations.stripe.prices.gift_1_month_brl,
						gift1MonthInr: master.integrations.stripe.prices.gift_1_month_inr,
						gift1MonthPln: master.integrations.stripe.prices.gift_1_month_pln,
						gift1MonthTry: master.integrations.stripe.prices.gift_1_month_try,
						gift1YearUsd: master.integrations.stripe.prices.gift_1_year_usd,
						gift1YearEur: master.integrations.stripe.prices.gift_1_year_eur,
						gift1YearBrl: master.integrations.stripe.prices.gift_1_year_brl,
						gift1YearInr: master.integrations.stripe.prices.gift_1_year_inr,
						gift1YearPln: master.integrations.stripe.prices.gift_1_year_pln,
						gift1YearTry: master.integrations.stripe.prices.gift_1_year_try,
					}
				: undefined,
			legacyPrices: master.integrations.stripe.legacy_prices,
		},
		cachePurge: {
			adapter: master.integrations.cache_purge.adapter,
			http: {
				endpoint: master.integrations.cache_purge.http.endpoint,
				token: master.integrations.cache_purge.http.token,
				timeoutMs: master.integrations.cache_purge.http.timeout_ms,
			},
		},
		clamav: {
			enabled: master.integrations.clamav.enabled,
			host: master.integrations.clamav.host,
			port: master.integrations.clamav.port,
			failOpen: master.integrations.clamav.fail_open,
		},
		ncmec: {
			enabled: master.integrations.ncmec.enabled,
			baseUrl: master.integrations.ncmec.base_url,
			username: master.integrations.ncmec.username,
			password: master.integrations.ncmec.password,
			reporterEmail: master.integrations.ncmec.reporter_email ?? '',
		},
		admin: {
			oauthClientSecret: master.services.admin.oauth_client_secret,
		},
		auth: {
			sudoModeSecret: master.auth.sudo_mode_secret,
			connectionInitiationSecret: master.auth.connection_initiation_secret,
			ssoAllowPrivateAddresses: master.auth.sso_allow_private_addresses,
			passkeys: {
				rpName: master.auth.passkeys.rp_name,
				rpId: master.auth.passkeys.rp_id,
				allowedOrigins: master.auth.passkeys.additional_allowed_origins,
			},
			vapid: {
				publicKey: master.auth.vapid.public_key,
				privateKey: master.auth.vapid.private_key,
				email: master.auth.vapid.email,
			},
			bluesky: master.auth.bluesky as BlueskyOAuthConfig,
		},
		klipy: {
			apiKey: master.integrations.klipy.api_key,
		},
		youtube: {
			apiKey: master.integrations.youtube.api_key,
		},
		instance: {
			selfHosted: master.instance.self_hosted,
			autoJoinInviteCode: master.instance.auto_join_invite_code,
			visionariesGuildId: master.instance.visionaries_guild_id,
			visionariesGuildVisionaryRoleId: master.instance.visionaries_guild_visionary_role_id,
			branding: {
				productName: master.instance.branding.product_name,
				iconUrl: master.instance.branding.icon_url,
				symbolUrl: master.instance.branding.symbol_url,
				logoUrl: master.instance.branding.logo_url,
				wordmarkUrl: master.instance.branding.wordmark_url,
				faviconUrl: master.instance.branding.favicon_url,
				themeColor: master.instance.branding.theme_color,
				statusPageUrl: master.instance.branding.status_page_url,
				statusPageIncidentHistoryUrl: master.instance.branding.status_page_incident_history_url,
			},
			setup: {
				configured: master.instance.setup.configured,
			},
		},
		discovery: {
			enabled: master.discovery.enabled,
			minMemberCount: master.discovery.min_member_count,
		},
		dev: {
			relaxRegistrationRateLimits: master.dev.relax_registration_rate_limits,
			disableRateLimits: master.dev.disable_rate_limits,
			testModeEnabled: master.dev.test_mode_enabled,
			testHarnessToken: master.dev.test_harness_token,
			validateResponses: resolveValidateResponses(master),
		},
		presignedAttachmentUploadsEnabled: master.services.api.presigned_attachment_uploads_enabled ?? false,
		presignedHarvestDownloadsEnabled: master.services.api.presigned_harvest_downloads_enabled ?? true,
		attachmentDecayEnabled: master.attachment_decay_enabled,
		deletionGracePeriodHours: master.dev.test_mode_enabled ? 0.01 : master.deletion_grace_period_hours,
		inactivityDeletionThresholdDays: master.inactivity_deletion_threshold_days,
		push: {
			publicVapidKey: master.auth.vapid.public_key,
			apns: {
				enabled: master.integrations.push.apns.enabled,
				teamId: master.integrations.push.apns.team_id,
				keyId: master.integrations.push.apns.key_id,
				privateKey: master.integrations.push.apns.private_key,
				privateKeyPath: master.integrations.push.apns.private_key_path,
				apps: mapApnsApps(master.integrations.push.apns.apps),
			},
		},
		appStore: {
			enabled: master.integrations.app_store.enabled,
			issuerId: master.integrations.app_store.issuer_id,
			keyId: master.integrations.app_store.key_id,
			privateKey: master.integrations.app_store.private_key,
			privateKeyPath: master.integrations.app_store.private_key_path,
			apps: (master.integrations.app_store.apps ?? []).map((app) => ({
				bundleId: app.bundle_id,
				appAppleId: app.app_apple_id,
			})),
			products: master.integrations.app_store.products ?? {},
		},
		googlePlay: {
			enabled: master.integrations.google_play.enabled,
			packages: master.integrations.google_play.packages ?? [],
			clientEmail: master.integrations.google_play.client_email,
			privateKey: master.integrations.google_play.private_key,
			privateKeyPath: master.integrations.google_play.private_key_path,
			serviceAccountJsonPath: master.integrations.google_play.service_account_json_path,
			tokenUri: master.integrations.google_play.token_uri ?? 'https://oauth2.googleapis.com/token',
			products: master.integrations.google_play.products ?? {},
			pushAudience: master.integrations.google_play.push_audience,
			pushServiceAccountEmail: master.integrations.google_play.push_service_account_email,
		},
		storeBilling: {
			sandboxUserIds: master.integrations.store_billing.sandbox_user_ids ?? [],
			sandboxEntitlesAll: master.integrations.store_billing.sandbox_entitles_all,
		},
		worker: {
			mode: apiWorkerConfig?.mode ?? 'all_lanes',
			laneName: apiWorkerConfig?.lane,
			taskName: apiWorkerConfig?.task as WorkerTaskName | undefined,
			enableCronScheduler: apiWorkerConfig?.enable_cron_scheduler,
			laneConcurrencyOverrides: {
				realtime: apiWorkerConfig?.lane_concurrency_overrides?.realtime,
				unfurl: apiWorkerConfig?.lane_concurrency_overrides?.unfurl,
				lifecycle: apiWorkerConfig?.lane_concurrency_overrides?.lifecycle,
				batch: apiWorkerConfig?.lane_concurrency_overrides?.batch,
			},
		},
	};
}

interface APIServerOptions {
	port: number;
	headersTimeoutMs: number;
	requestTimeoutMs: number;
}

export function buildAPIServerOptions(config: APIConfig): APIServerOptions {
	return {
		port: config.port,
		headersTimeoutMs: config.headersTimeoutMs,
		requestTimeoutMs: config.requestTimeoutMs,
	};
}

let _config: APIConfig | null = null;

export function initializeConfig(config: APIConfig): void {
	if (_config !== null) {
		return;
	}
	_config = config;
}

export function getConfig(): APIConfig {
	if (_config === null) {
		throw new Error('Config has not been initialized. Call initializeConfig() first.');
	}
	return _config;
}

export const Config: APIConfig = new Proxy({} as APIConfig, {
	get(_target, prop: keyof APIConfig | symbol) {
		if (_config === null) {
			throw new Error('Config has not been initialized. Call initializeConfig() first.');
		}
		return _config[prop as keyof APIConfig];
	},
	set() {
		throw new Error('Cannot modify Config directly. Use initializeConfig() instead.');
	},
});
