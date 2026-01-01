/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import process from 'node:process';

import {z} from '~/Schema';

function required(key: string): string {
	const value = process.env[key];
	if (!value) {
		throw new Error(`Missing required environment variable: ${key}`);
	}
	return value;
}

function optional(key: string): string | undefined {
	return process.env[key] || undefined;
}

function optionalInt(key: string, defaultValue: number): number {
	const value = process.env[key];
	if (!value) return defaultValue;
	const parsed = Number.parseInt(value, 10);
	return Number.isNaN(parsed) ? defaultValue : parsed;
}

function optionalBool(key: string, defaultValue = false): boolean {
	const value = process.env[key];
	if (!value) return defaultValue;
	return value.toLowerCase() === 'true' || value === '1';
}

function extractHostname(url: string): string {
	try {
		return new URL(url).hostname;
	} catch {
		throw new Error(`Invalid URL: ${url}`);
	}
}

function trimTrailingSlash(value: string): string {
	if (value.length > 1 && value.endsWith('/')) {
		return trimTrailingSlash(value.slice(0, -1));
	}
	return value;
}

function normalizePath(path: string): string {
	const trimmed = path.trim();
	if (trimmed === '' || trimmed === '/') return '';
	const withLeadingSlash = trimmed.startsWith('/') ? trimmed : `/${trimmed}`;
	return trimTrailingSlash(withLeadingSlash);
}

function appendPath(endpoint: string, path: string): string {
	const cleanEndpoint = trimTrailingSlash(endpoint);
	const normalizedPath = normalizePath(path);
	return normalizedPath ? `${cleanEndpoint}${normalizedPath}` : cleanEndpoint;
}

function parseCommaSeparated(value: string): Array<string> {
	return value
		.split(',')
		.map((item) => item.trim())
		.filter((item) => item.length > 0);
}

const ConfigSchema = z.object({
	nodeEnv: z.enum(['development', 'production']),
	port: z.number(),

	postgres: z.object({
		url: z.string(),
	}),

	cassandra: z.object({
		hosts: z.string(),
		keyspace: z.string(),
		localDc: z.string(),
		username: z.string(),
		password: z.string(),
	}),

	redis: z.object({
		url: z.string(),
	}),

	gateway: z.object({
		rpcHost: z.string(),
		rpcPort: z.number(),
		rpcSecret: z.string(),
	}),

	mediaProxy: z.object({
		host: z.string(),
		port: z.number(),
		secretKey: z.string(),
	}),

	geoip: z.object({
		provider: z.enum(['ipinfo', 'maxmind']),
		host: z.string().optional(),
		maxmindDbPath: z.string().optional(),
	}),

	endpoints: z.object({
		apiPublic: z.string(),
		apiClient: z.string(),
		webApp: z.string(),
		gateway: z.string(),
		media: z.string(),
		cdn: z.string(),
		marketing: z.string(),
		admin: z.string(),
		invite: z.string(),
		gift: z.string(),
	}),

	hosts: z.object({
		invite: z.string(),
		gift: z.string(),
		marketing: z.string(),
		unfurlIgnored: z.array(z.string()),
	}),

	s3: z.object({
		endpoint: z.string(),
		accessKeyId: z.string(),
		secretAccessKey: z.string(),
		buckets: z.object({
			cdn: z.string(),
			uploads: z.string(),
			reports: z.string(),
			harvests: z.string(),
			downloads: z.string(),
		}),
	}),

	email: z.object({
		enabled: z.boolean(),
		apiKey: z.string().optional(),
		webhookPublicKey: z.string().optional(),
		fromEmail: z.string(),
		fromName: z.string(),
	}),

	sms: z.object({
		enabled: z.boolean(),
		accountSid: z.string().optional(),
		authToken: z.string().optional(),
		verifyServiceSid: z.string().optional(),
	}),

	captcha: z.object({
		enabled: z.boolean(),
		provider: z.enum(['hcaptcha', 'turnstile', 'none']),
		hcaptcha: z
			.object({
				siteKey: z.string(),
				secretKey: z.string(),
			})
			.optional(),
		turnstile: z
			.object({
				siteKey: z.string(),
				secretKey: z.string(),
			})
			.optional(),
	}),

	voice: z.object({
		enabled: z.boolean(),
		apiKey: z.string().optional(),
		apiSecret: z.string().optional(),
		webhookUrl: z.string().optional(),
		url: z.string().optional(),
		autoCreateDummyData: z.boolean(),
	}),

	search: z.object({
		enabled: z.boolean(),
		url: z.string().optional(),
		apiKey: z.string().optional(),
	}),

	stripe: z.object({
		enabled: z.boolean(),
		secretKey: z.string().optional(),
		webhookSecret: z.string().optional(),
		prices: z
			.object({
				monthlyUsd: z.string().optional(),
				monthlyEur: z.string().optional(),
				yearlyUsd: z.string().optional(),
				yearlyEur: z.string().optional(),
				visionaryUsd: z.string().optional(),
				visionaryEur: z.string().optional(),
				giftVisionaryUsd: z.string().optional(),
				giftVisionaryEur: z.string().optional(),
				gift1MonthUsd: z.string().optional(),
				gift1MonthEur: z.string().optional(),
				gift1YearUsd: z.string().optional(),
				gift1YearEur: z.string().optional(),
			})
			.optional(),
	}),

	cloudflare: z.object({
		purgeEnabled: z.boolean(),
		zoneId: z.string().optional(),
		apiToken: z.string().optional(),
	}),

	alerts: z.object({
		webhookUrl: z.string().url().optional(),
	}),

	clamav: z.object({
		enabled: z.boolean(),
		host: z.string(),
		port: z.number(),
		failOpen: z.boolean(),
	}),

	adminOauth2: z.object({
		clientId: z.string().optional(),
		clientSecret: z.string().optional(),
		redirectUri: z.string(),
		autoCreate: z.boolean(),
	}),

	auth: z.object({
		sudoModeSecret: z.string(),
		passkeys: z.object({
			rpName: z.string(),
			rpId: z.string(),
			allowedOrigins: z.array(z.string()),
		}),
	}),

	cookie: z.object({
		domain: z.string(),
		secure: z.boolean(),
	}),

	tenor: z.object({
		apiKey: z.string().optional(),
	}),

	youtube: z.object({
		apiKey: z.string().optional(),
	}),

	instance: z.object({
		selfHosted: z.boolean(),
		autoJoinInviteCode: z.string().optional(),
		visionariesGuildId: z.string().optional(),
		operatorsGuildId: z.string().optional(),
	}),

	dev: z.object({
		relaxRegistrationRateLimits: z.boolean(),
		disableRateLimits: z.boolean(),
		testModeEnabled: z.boolean(),
		testHarnessToken: z.string().optional(),
	}),

	attachmentDecayEnabled: z.boolean(),

	deletionGracePeriodHours: z.number(),
	inactivityDeletionThresholdDays: z.number().optional(),

	push: z.object({
		publicVapidKey: z.string().optional(),
	}),

	metrics: z.object({
		host: z.string().optional(),
	}),
});

function loadConfig() {
	const apiPublicEndpoint = required('FLUXER_API_PUBLIC_ENDPOINT');
	const apiClientEndpoint = optional('FLUXER_API_CLIENT_ENDPOINT') || apiPublicEndpoint;
	const webAppEndpoint = required('FLUXER_APP_ENDPOINT');
	const gatewayEndpoint = required('FLUXER_GATEWAY_ENDPOINT');
	const mediaEndpoint = required('FLUXER_MEDIA_ENDPOINT');
	const cdnEndpoint = required('FLUXER_CDN_ENDPOINT');
	const marketingEndpoint = appendPath(required('FLUXER_MARKETING_ENDPOINT'), required('FLUXER_PATH_MARKETING'));
	const adminEndpoint = appendPath(required('FLUXER_ADMIN_ENDPOINT'), required('FLUXER_PATH_ADMIN'));
	const inviteEndpoint = required('FLUXER_INVITE_ENDPOINT');
	const giftEndpoint = required('FLUXER_GIFT_ENDPOINT');

	const passkeyOriginsEnv = optional('PASSKEY_ALLOWED_ORIGINS');
	const passkeyAllowedOrigins = passkeyOriginsEnv
		? parseCommaSeparated(passkeyOriginsEnv)
		: Array.from(new Set([apiPublicEndpoint, webAppEndpoint, apiClientEndpoint]));

	const testModeEnabled = optionalBool('FLUXER_TEST_MODE');
	const geoipProviderRaw = optional('GEOIP_PROVIDER')?.trim().toLowerCase();
	const geoipProvider = geoipProviderRaw === 'maxmind' ? 'maxmind' : 'ipinfo';
	const maxmindDbPath = optional('MAXMIND_DB_PATH');
	if (geoipProvider === 'maxmind' && !maxmindDbPath) {
		throw new Error('Missing required environment variable: MAXMIND_DB_PATH');
	}

	return ConfigSchema.parse({
		nodeEnv: optional('NODE_ENV') || 'development',
		port: optionalInt('FLUXER_API_PORT', 8080),

		postgres: {
			url: required('DATABASE_URL'),
		},

		cassandra: {
			hosts: required('CASSANDRA_HOSTS'),
			keyspace: required('CASSANDRA_KEYSPACE'),
			localDc: optional('CASSANDRA_LOCAL_DC'),
			username: required('CASSANDRA_USERNAME'),
			password: required('CASSANDRA_PASSWORD'),
		},

		redis: {
			url: required('REDIS_URL'),
		},

		gateway: {
			rpcHost: optional('FLUXER_GATEWAY_RPC_HOST') || 'gateway',
			rpcPort: optionalInt('FLUXER_GATEWAY_RPC_PORT', 8081),
			rpcSecret: required('GATEWAY_RPC_SECRET'),
		},

		mediaProxy: {
			host: optional('FLUXER_MEDIA_PROXY_HOST') || 'media',
			port: optionalInt('FLUXER_MEDIA_PROXY_PORT', 8080),
			secretKey: required('MEDIA_PROXY_SECRET_KEY'),
		},

		geoip: {
			provider: geoipProvider,
			host: optional('GEOIP_HOST') || 'geoip',
			maxmindDbPath,
		},

		endpoints: {
			apiPublic: apiPublicEndpoint,
			apiClient: apiClientEndpoint,
			webApp: webAppEndpoint,
			gateway: gatewayEndpoint,
			media: mediaEndpoint,
			cdn: cdnEndpoint,
			marketing: marketingEndpoint,
			admin: adminEndpoint,
			invite: inviteEndpoint,
			gift: giftEndpoint,
		},

		hosts: {
			invite: extractHostname(inviteEndpoint),
			gift: extractHostname(giftEndpoint),
			marketing: extractHostname(marketingEndpoint),
			unfurlIgnored: parseCommaSeparated(optional('FLUXER_UNFURL_IGNORED_HOSTS') || '').concat([
				'web.fluxer.app',
				'web.canary.fluxer.app',
			]),
		},

		s3: {
			endpoint: required('AWS_S3_ENDPOINT'),
			accessKeyId: required('AWS_ACCESS_KEY_ID'),
			secretAccessKey: required('AWS_SECRET_ACCESS_KEY'),
			buckets: {
				cdn: required('AWS_S3_BUCKET_CDN'),
				uploads: required('AWS_S3_BUCKET_UPLOADS'),
				reports: required('AWS_S3_BUCKET_REPORTS'),
				harvests: required('AWS_S3_BUCKET_HARVESTS'),
				downloads: required('AWS_S3_BUCKET_DOWNLOADS'),
			},
		},

		email: {
			enabled: optionalBool('EMAIL_ENABLED'),
			apiKey: optional('SENDGRID_API_KEY'),
			webhookPublicKey: optional('SENDGRID_WEBHOOK_PUBLIC_KEY'),
			fromEmail: optional('SENDGRID_FROM_EMAIL') || 'noreply@fluxer.app',
			fromName: optional('SENDGRID_FROM_NAME') || 'Fluxer',
		},

		sms: {
			enabled: optionalBool('SMS_ENABLED'),
			accountSid: optional('TWILIO_ACCOUNT_SID'),
			authToken: optional('TWILIO_AUTH_TOKEN'),
			verifyServiceSid: optional('TWILIO_VERIFY_SERVICE_SID'),
		},

		captcha: {
			enabled: optionalBool('CAPTCHA_ENABLED'),
			provider: (optional('CAPTCHA_PRIMARY_PROVIDER') as 'hcaptcha' | 'turnstile' | 'none') || 'none',
			hcaptcha:
				optional('HCAPTCHA_SITE_KEY') && optional('HCAPTCHA_SECRET_KEY')
					? {
							siteKey: required('HCAPTCHA_SITE_KEY'),
							secretKey: required('HCAPTCHA_SECRET_KEY'),
						}
					: undefined,
			turnstile:
				optional('TURNSTILE_SITE_KEY') && optional('TURNSTILE_SECRET_KEY')
					? {
							siteKey: required('TURNSTILE_SITE_KEY'),
							secretKey: required('TURNSTILE_SECRET_KEY'),
						}
					: undefined,
		},

		voice: {
			enabled: optionalBool('VOICE_ENABLED'),
			apiKey: optional('LIVEKIT_API_KEY'),
			apiSecret: optional('LIVEKIT_API_SECRET'),
			webhookUrl: optional('LIVEKIT_WEBHOOK_URL'),
			url: optional('LIVEKIT_URL'),
			autoCreateDummyData: optionalBool('LIVEKIT_AUTO_CREATE_DUMMY_DATA'),
		},

		search: {
			enabled: optionalBool('SEARCH_ENABLED'),
			url: optional('MEILISEARCH_URL'),
			apiKey: optional('MEILISEARCH_API_KEY'),
		},

		stripe: {
			enabled: optionalBool('STRIPE_ENABLED'),
			secretKey: optional('STRIPE_SECRET_KEY'),
			webhookSecret: optional('STRIPE_WEBHOOK_SECRET'),
			prices: optionalBool('STRIPE_ENABLED')
				? {
						monthlyUsd: optional('STRIPE_PRICE_ID_MONTHLY_USD'),
						monthlyEur: optional('STRIPE_PRICE_ID_MONTHLY_EUR'),
						yearlyUsd: optional('STRIPE_PRICE_ID_YEARLY_USD'),
						yearlyEur: optional('STRIPE_PRICE_ID_YEARLY_EUR'),
						visionaryUsd: optional('STRIPE_PRICE_ID_VISIONARY_USD'),
						visionaryEur: optional('STRIPE_PRICE_ID_VISIONARY_EUR'),
						giftVisionaryUsd: optional('STRIPE_PRICE_ID_GIFT_VISIONARY_USD'),
						giftVisionaryEur: optional('STRIPE_PRICE_ID_GIFT_VISIONARY_EUR'),
						gift1MonthUsd: optional('STRIPE_PRICE_ID_GIFT_1_MONTH_USD'),
						gift1MonthEur: optional('STRIPE_PRICE_ID_GIFT_1_MONTH_EUR'),
						gift1YearUsd: optional('STRIPE_PRICE_ID_GIFT_1_YEAR_USD'),
						gift1YearEur: optional('STRIPE_PRICE_ID_GIFT_1_YEAR_EUR'),
					}
				: undefined,
		},

		cloudflare: {
			purgeEnabled: optionalBool('CLOUDFLARE_PURGE_ENABLED'),
			zoneId: optional('CLOUDFLARE_ZONE_ID'),
			apiToken: optional('CLOUDFLARE_API_TOKEN'),
		},

		alerts: {
			webhookUrl: optional('ALERT_WEBHOOK_URL'),
		},

		clamav: {
			enabled: optionalBool('CLAMAV_ENABLED'),
			host: optional('CLAMAV_HOST') || 'clamav',
			port: optionalInt('CLAMAV_PORT', 3310),
			failOpen: optionalBool('CLAMAV_FAIL_OPEN', true),
		},

		adminOauth2: {
			clientId: optional('ADMIN_OAUTH2_CLIENT_ID'),
			clientSecret: optional('ADMIN_OAUTH2_CLIENT_SECRET'),
			redirectUri: `${adminEndpoint}/oauth2_callback`,
			autoCreate: optionalBool('ADMIN_OAUTH2_AUTO_CREATE'),
		},

		auth: {
			sudoModeSecret: required('SUDO_MODE_SECRET'),
			passkeys: {
				rpName: optional('PASSKEY_RP_NAME') || 'Fluxer',
				rpId: optional('PASSKEY_RP_ID') || extractHostname(webAppEndpoint),
				allowedOrigins: passkeyAllowedOrigins,
			},
		},

		cookie: {
			domain: optional('FLUXER_COOKIE_DOMAIN') || '',
			secure: optionalBool('FLUXER_COOKIE_SECURE', true),
		},

		tenor: {
			apiKey: optional('TENOR_API_KEY'),
		},

		youtube: {
			apiKey: optional('YOUTUBE_API_KEY'),
		},

		instance: {
			selfHosted: optionalBool('SELF_HOSTED'),
			autoJoinInviteCode: optional('AUTO_JOIN_INVITE_CODE'),
			visionariesGuildId: optional('FLUXER_VISIONARIES_GUILD_ID'),
			operatorsGuildId: optional('FLUXER_OPERATORS_GUILD_ID'),
		},

		dev: {
			relaxRegistrationRateLimits: optionalBool('RELAX_REGISTRATION_RATE_LIMITS'),
			disableRateLimits: optionalBool('DISABLE_RATE_LIMITS'),
			testModeEnabled,
			testHarnessToken: optional('FLUXER_TEST_TOKEN'),
		},

		attachmentDecayEnabled: optionalBool('ATTACHMENT_DECAY_ENABLED', true),

		deletionGracePeriodHours: testModeEnabled ? 0.01 : 336,
		inactivityDeletionThresholdDays: optionalInt('INACTIVITY_DELETION_THRESHOLD_DAYS', 365 * 2),

		push: {
			publicVapidKey: optional('VAPID_PUBLIC_KEY'),
		},

		metrics: {
			host: optional('FLUXER_METRICS_HOST'),
		},
	});
}

export const Config = loadConfig();

export type Config = z.infer<typeof ConfigSchema>;
