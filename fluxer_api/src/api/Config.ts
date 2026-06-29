// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {parseGeoipSourceConfig, resolveGeoipRuntimeSourceConfig} from '@pkgs/geoip/src/GeoipStartup';
import type {APIConfig, BlueskyOAuthConfig} from './config/APIConfig';
import type {WorkerTaskName} from './worker/WorkerLaneConfig';

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

function resolveGatewayInternalUrl(master: MasterConfig): string {
	const configuredInternalGateway = (
		master.internal as {
			gateway?: string;
		}
	).gateway;
	if (typeof configuredInternalGateway === 'string' && configuredInternalGateway.length > 0) {
		return trimTrailingSlash(configuredInternalGateway);
	}
	try {
		const gatewayUrl = new URL(master.endpoints.gateway);
		if (gatewayUrl.protocol === 'ws:') {
			gatewayUrl.protocol = 'http:';
		} else if (gatewayUrl.protocol === 'wss:') {
			gatewayUrl.protocol = 'https:';
		}
		return trimTrailingSlash(gatewayUrl.toString());
	} catch {
		throw new Error(`Invalid gateway endpoint URL: ${master.endpoints.gateway}`);
	}
}

function isBoolean(value: unknown): value is boolean {
	return typeof value === 'boolean';
}

function resolveTrustClientIpHeader(proxyConfig: object): boolean {
	const configuredValue = Reflect.get(proxyConfig, 'trust_client_ip_header');
	if (isBoolean(configuredValue)) {
		return configuredValue;
	}
	return false;
}

function mapPushProviderApps(
	apps:
		| Array<{
				app_id?: string;
				topic?: string;
				environment?: 'production' | 'development';
				project_id?: string;
		  }>
		| undefined,
): APIConfig['push']['apns']['apps'] {
	return (apps ?? []).flatMap((app) => {
		if (!app.app_id) return [];
		return [
			{
				appId: app.app_id,
				topic: app.topic,
				environment: app.environment,
				projectId: app.project_id,
			},
		];
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
	const uploadRelaySecretBase64 = process.env.FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 ?? '';
	if (!s3Config) {
		throw new Error('S3 configuration is required for the API');
	}
	const s3Buckets = s3Config.buckets ?? {
		cdn: '',
		uploads: '',
		downloads: '',
		reports: '',
		harvests: '',
		static: '',
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
		},
		database: {
			backend: master.database.backend,
		},
		kv: {
			provider: 'redis' as const,
			url: master.internal.kv,
			mode: ((
				master.internal as {
					kv_mode?: string;
				}
			).kv_mode ?? 'standalone') as 'standalone' | 'cluster',
			clusterNodes:
				(
					master.internal as {
						kv_cluster_nodes?: Array<{
							host: string;
							port: number;
						}>;
					}
				).kv_cluster_nodes ?? [],
			clusterNatMap:
				(
					master.internal as {
						kv_cluster_nat_map?: Record<
							string,
							{
								host: string;
								port: number;
							}
						>;
					}
				).kv_cluster_nat_map ?? {},
		},
		nats: {
			coreUrl: master.services.nats?.core_url ?? 'nats://127.0.0.1:4222',
			jetStreamUrl: master.services.nats?.jetstream_url ?? 'nats://127.0.0.1:4223',
			authToken: master.services.nats?.auth_token ?? '',
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
			gateway: master.endpoints.gateway,
			media: master.endpoints.media,
			marketing: master.endpoints.marketing,
			admin: master.endpoints.admin,
			invite: master.endpoints.invite,
			gift: master.endpoints.gift,
			staticCdn: master.endpoints.static_cdn,
		},
		internal: {
			gateway: resolveGatewayInternalUrl(master),
			gatewayRpcAuthToken: master.services.gateway.rpc_auth_token ?? '',
		},
		hosts: {
			invite: extractHostname(master.endpoints.invite),
			gift: extractHostname(master.endpoints.gift),
			marketing: extractHostname(master.endpoints.marketing),
			unfurlIgnored: master.services.api.unfurl_ignored_hosts,
		},
		embeds: {
			oEmbedHtmlEnabled: master.services.api.embeds.oembed_html_enabled,
			oEmbedHtmlAllowUntrustedOnSelfHosted: master.services.api.embeds.oembed_html_allow_untrusted_on_self_hosted,
			oEmbedHtmlAllowedHosts: master.services.api.embeds.oembed_html_allowed_hosts,
			cacheDefaultTtlSeconds: master.services.api.embeds.cache_default_ttl_seconds,
			cacheMaxTtlSeconds: master.services.api.embeds.cache_max_ttl_seconds,
			cacheMinTtlSeconds: master.services.api.embeds.cache_min_ttl_seconds,
			cacheRespectRemoteTtl: master.services.api.embeds.cache_respect_remote_ttl,
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
		sms: {
			enabled: master.integrations.sms.enabled,
			accountSid: master.integrations.sms.account_sid,
			authToken: master.integrations.sms.auth_token,
			verifyServiceSid: master.integrations.sms.verify_service_sid,
			inboundChallengeNumber: master.integrations.sms.inbound_challenge_number || undefined,
			inboundWebhookAuthToken: master.integrations.sms.inbound_webhook_auth_token || master.integrations.sms.auth_token,
			inboundWebhookPublicUrl: master.integrations.sms.inbound_webhook_public_url || undefined,
		},
		risk: {
			enabled: master.integrations.risk_integration.enabled,
			ipinfoApiKey: master.integrations.risk_integration.ipinfo_api_key || undefined,
			accountPolicyDsl: master.integrations.risk_integration.account_policy_dsl,
		},
		captcha: {
			enabled: master.integrations.captcha.enabled,
			provider: master.integrations.captcha.provider,
			hcaptcha: master.integrations.captcha.hcaptcha
				? {
						siteKey: master.integrations.captcha.hcaptcha.site_key,
						secretKey: master.integrations.captcha.hcaptcha.secret_key,
					}
				: undefined,
			turnstile: master.integrations.captcha.turnstile
				? {
						siteKey: master.integrations.captcha.turnstile.site_key,
						secretKey: master.integrations.captcha.turnstile.secret_key,
					}
				: undefined,
		},
		contentModeration: {
			nsfwThreshold: master.services.api.content_moderation?.nsfw_threshold ?? 0.7,
		},
		voice: {
			enabled: master.integrations.voice.enabled,
			apiKey: master.integrations.voice.api_key,
			apiSecret: master.integrations.voice.api_secret,
			webhookUrl: master.integrations.voice.webhook_url,
			url: master.integrations.voice.url,
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
						monthlyInr: master.integrations.stripe.prices.monthly_inr,
						monthlyPln: master.integrations.stripe.prices.monthly_pln,
						monthlyTry: master.integrations.stripe.prices.monthly_try,
						yearlyUsd: master.integrations.stripe.prices.yearly_usd,
						yearlyEur: master.integrations.stripe.prices.yearly_eur,
						yearlyBrl: master.integrations.stripe.prices.yearly_brl,
						yearlyInr: master.integrations.stripe.prices.yearly_inr,
						yearlyPln: master.integrations.stripe.prices.yearly_pln,
						yearlyTry: master.integrations.stripe.prices.yearly_try,
						gift1MonthUsd: master.integrations.stripe.prices.gift_1_month_usd,
						gift1MonthEur: master.integrations.stripe.prices.gift_1_month_eur,
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
		},
		bunny: {
			purgeEnabled: master.integrations.bunny.purge_enabled,
			apiKey: master.integrations.bunny.api_key,
			pullZoneId: master.integrations.bunny.pull_zone_id,
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
			basePath: master.services.admin.base_path,
			oauthClientSecret: master.services.admin.oauth_client_secret,
		},
		auth: {
			sudoModeSecret: master.auth.sudo_mode_secret,
			connectionInitiationSecret: master.auth.connection_initiation_secret,
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
		cookie: master.cookie,
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
			branding: {
				productName: master.instance.branding.product_name,
				iconUrl: master.instance.branding.icon_url,
				symbolUrl: master.instance.branding.symbol_url,
				logoUrl: master.instance.branding.logo_url,
				wordmarkUrl: master.instance.branding.wordmark_url,
				faviconUrl: master.instance.branding.favicon_url,
				themeColor: master.instance.branding.theme_color,
			},
			setup: {
				configured: master.instance.setup.configured,
			},
		},
		abusePolicy: {
			inboundPhoneCountryCodes: master.instance.abuse_policy.inbound_phone_country_codes,
			phoneVerification: {
				inboundRequiredPrefixes: master.instance.abuse_policy.phone_verification.inbound_required_prefixes,
			},
			directContactSpam: {
				enabled: master.instance.abuse_policy.direct_contact_spam.enabled,
				countryCodes: master.instance.abuse_policy.direct_contact_spam.country_codes,
				distinctTargetThreshold: master.instance.abuse_policy.direct_contact_spam.distinct_target_threshold,
				targetWindowMs: master.instance.abuse_policy.direct_contact_spam.target_window_ms,
				action: master.instance.abuse_policy.direct_contact_spam.action,
			},
		},
		domain: {
			baseDomain: master.domain.base_domain,
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
		},
		presignedAttachmentUploadsEnabled: master.services.api.presigned_attachment_uploads_enabled ?? false,
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
				defaultEnvironment: master.integrations.push.apns.default_environment ?? 'production',
				apps: mapPushProviderApps(master.integrations.push.apns.apps),
			},
			fcm: {
				enabled: master.integrations.push.fcm.enabled,
				projectId: master.integrations.push.fcm.project_id,
				clientEmail: master.integrations.push.fcm.client_email,
				privateKey: master.integrations.push.fcm.private_key,
				privateKeyPath: master.integrations.push.fcm.private_key_path,
				serviceAccountJsonPath: master.integrations.push.fcm.service_account_json_path,
				tokenUri: master.integrations.push.fcm.token_uri ?? 'https://oauth2.googleapis.com/token',
				apps: mapPushProviderApps(master.integrations.push.fcm.apps),
			},
		},
		worker: {
			mode: apiWorkerConfig?.mode ?? 'all_lanes',
			laneName: apiWorkerConfig?.lane,
			taskName: apiWorkerConfig?.task as WorkerTaskName | undefined,
			enableCronScheduler: apiWorkerConfig?.enable_cron_scheduler,
			enableVoiceReconciliation: apiWorkerConfig?.enable_voice_reconciliation ?? true,
			laneConcurrencyOverrides: {
				realtime: apiWorkerConfig?.lane_concurrency_overrides?.realtime,
				unfurl: apiWorkerConfig?.lane_concurrency_overrides?.unfurl,
				lifecycle: apiWorkerConfig?.lane_concurrency_overrides?.lifecycle,
				batch: apiWorkerConfig?.lane_concurrency_overrides?.batch,
			},
		},
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
