// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskName} from '../worker/WorkerLaneConfig';

export type APIWorkerMode = 'all_lanes' | 'single_lane' | 'single_task';
export type APIWorkerLaneName = 'realtime' | 'unfurl' | 'lifecycle' | 'batch';
export type PushProviderEnvironment = 'production' | 'development';

export interface PushProviderAppConfig {
	appId: string;
	topic?: string;
	environment?: PushProviderEnvironment;
	projectId?: string;
}

interface APIGeoipFilesystemConfig {
	mode: 'filesystem';
	maxmindDbPath?: string;
	maxmindAsnDbPath?: string;
}

interface APIGeoipS3Config {
	mode: 's3';
	maxmindDbPath: string;
	maxmindAsnDbPath?: string;
	s3Bucket: string;
	s3Key: string;
	s3AsnKey?: string;
}

export type APIGeoipConfig = APIGeoipFilesystemConfig | APIGeoipS3Config;

export interface APIConfig {
	nodeEnv: 'development' | 'production';
	port: number;
	cassandra: {
		hosts: string;
		port: number;
		keyspace: string;
		localDc: string;
		username: string;
		password: string;
	};
	postgres: {
		url: string;
		host: string;
		port: number;
		database: string;
		username: string;
		password: string;
		ssl: boolean;
		sslCa: string;
		maxConnections: number;
		kvTable: string;
	};
	database: {
		backend: 'cassandra' | 'postgres';
	};
	kv: {
		provider: 'redis';
		url: string;
		mode: 'standalone' | 'cluster';
		clusterNodes: Array<{
			host: string;
			port: number;
		}>;
		clusterNatMap: Record<
			string,
			{
				host: string;
				port: number;
			}
		>;
	};
	nats: {
		coreUrl: string;
		jetStreamUrl: string;
		authToken: string;
	};
	search: {
		engine: 'elasticsearch' | 'meilisearch';
		url: string;
		apiKey: string;
		username: string;
		password: string;
		tlsRejectUnauthorized: boolean;
	};
	mediaProxy: {
		host: string;
		port: number;
		secretKey: string;
		uploadRelay: {
			endpoint: string;
			relaySecretBase64: string;
			maxBodyBytes: number;
			tokenTtlSecs: number;
			keepDirectCountries: Array<string>;
		};
	};
	geoip: APIGeoipConfig;
	proxy: {
		trust_client_ip_header: boolean;
		client_ip_header: string;
	};
	endpoints: {
		apiPublic: string;
		apiClient: string;
		webApp: string;
		gateway: string;
		media: string;
		staticCdn: string;
		marketing: string;
		admin: string;
		invite: string;
		gift: string;
	};
	internal: {
		gateway: string;
		gatewayRpcAuthToken: string;
	};
	hosts: {
		invite: string;
		gift: string;
		marketing: string;
		unfurlIgnored: Array<string>;
	};
	embeds: {
		oEmbedHtmlEnabled: boolean;
		oEmbedHtmlAllowUntrustedOnSelfHosted: boolean;
		oEmbedHtmlAllowedHosts: Array<string>;
		cacheDefaultTtlSeconds: number;
		cacheMaxTtlSeconds: number;
		cacheMinTtlSeconds: number;
		cacheRespectRemoteTtl: boolean;
	};
	s3: {
		endpoint: string;
		presignedUrlBase: string | undefined;
		forcePathStyle: boolean;
		region: string;
		accessKeyId: string;
		secretAccessKey: string;
		buckets: {
			cdn: string;
			uploads: string;
			reports: string;
			harvests: string;
			downloads: string;
			static: string;
		};
	};
	email: {
		enabled: boolean;
		provider: 'smtp' | 'none';
		webhookSecret?: string;
		fromEmail: string;
		fromName: string;
		smtp?: {
			host: string;
			port: number;
			username: string;
			password: string;
			secure: boolean;
		};
	};
	sms: {
		enabled: boolean;
		accountSid?: string;
		authToken?: string;
		verifyServiceSid?: string;
		inboundChallengeNumber?: string;
		inboundWebhookAuthToken?: string;
		inboundWebhookPublicUrl?: string;
	};
	risk: {
		enabled: boolean;
		ipinfoApiKey?: string;
		accountPolicyDsl?: unknown;
	};
	captcha: {
		enabled: boolean;
		provider: 'hcaptcha' | 'turnstile' | 'none';
		hcaptcha?: {
			siteKey: string;
			secretKey: string;
		};
		turnstile?: {
			siteKey: string;
			secretKey: string;
		};
	};
	contentModeration: {
		nsfwThreshold: number;
	};
	voice: {
		enabled: boolean;
		apiKey?: string;
		apiSecret?: string;
		webhookUrl?: string;
		url?: string;
		defaultRegion?: {
			id: string;
			name: string;
			emoji: string;
			latitude: number;
			longitude: number;
		};
	};
	stripe: {
		enabled: boolean;
		secretKey?: string;
		webhookSecret?: string;
		prices?: {
			monthlyUsd?: string;
			monthlyEur?: string;
			monthlyBrl?: string;
			monthlyInr?: string;
			monthlyPln?: string;
			monthlyTry?: string;
			yearlyUsd?: string;
			yearlyEur?: string;
			yearlyBrl?: string;
			yearlyInr?: string;
			yearlyPln?: string;
			yearlyTry?: string;
			gift1MonthUsd?: string;
			gift1MonthEur?: string;
			gift1MonthBrl?: string;
			gift1MonthInr?: string;
			gift1MonthPln?: string;
			gift1MonthTry?: string;
			gift1YearUsd?: string;
			gift1YearEur?: string;
			gift1YearBrl?: string;
			gift1YearInr?: string;
			gift1YearPln?: string;
			gift1YearTry?: string;
		};
	};
	bunny: {
		purgeEnabled: boolean;
		apiKey?: string;
		pullZoneId?: number;
	};
	clamav: {
		enabled: boolean;
		host: string;
		port: number;
		failOpen: boolean;
	};
	admin: {
		basePath: string;
		oauthClientSecret?: string;
	};
	auth: {
		sudoModeSecret: string;
		connectionInitiationSecret: string;
		passkeys: {
			rpName: string;
			rpId: string;
			allowedOrigins: Array<string>;
		};
		vapid: {
			publicKey: string;
			privateKey: string;
			email?: string;
		};
		bluesky: BlueskyOAuthConfig;
	};
	cookie: {
		domain: string;
		secure: boolean;
	};
	klipy: {
		apiKey?: string;
	};
	youtube: {
		apiKey?: string;
	};
	instance: {
		selfHosted: boolean;
		autoJoinInviteCode?: string;
		visionariesGuildId?: string;
		visionariesGuildVisionaryRoleId?: string;
		branding: {
			productName: string;
			iconUrl?: string;
			symbolUrl?: string;
			logoUrl?: string;
			wordmarkUrl?: string;
			faviconUrl?: string;
			themeColor?: string;
		};
		setup: {
			configured: boolean;
		};
	};
	abusePolicy: {
		inboundPhoneCountryCodes: Array<string>;
		phoneVerification: {
			inboundRequiredPrefixes: Array<string>;
		};
		directContactSpam: {
			enabled: boolean;
			countryCodes: Array<string>;
			distinctTargetThreshold: number;
			targetWindowMs: number;
			action: 'flag_spammer' | 'suppress_delivery';
		};
	};
	domain: {
		baseDomain: string;
	};
	discovery: {
		enabled: boolean;
		minMemberCount: number;
	};
	dev: {
		relaxRegistrationRateLimits: boolean;
		disableRateLimits: boolean;
		testModeEnabled: boolean;
		testHarnessToken?: string;
	};
	presignedAttachmentUploadsEnabled: boolean;
	attachmentDecayEnabled: boolean;
	deletionGracePeriodHours: number;
	inactivityDeletionThresholdDays?: number;
	push: {
		publicVapidKey?: string;
		apns: {
			enabled: boolean;
			teamId?: string;
			keyId?: string;
			privateKey?: string;
			privateKeyPath?: string;
			defaultEnvironment: PushProviderEnvironment;
			apps: Array<PushProviderAppConfig>;
		};
		fcm: {
			enabled: boolean;
			projectId?: string;
			clientEmail?: string;
			privateKey?: string;
			privateKeyPath?: string;
			serviceAccountJsonPath?: string;
			tokenUri: string;
			apps: Array<PushProviderAppConfig>;
		};
	};
	worker: {
		mode: APIWorkerMode;
		laneName?: APIWorkerLaneName;
		taskName?: WorkerTaskName;
		enableCronScheduler?: boolean;
		enableVoiceReconciliation: boolean;
		laneConcurrencyOverrides: {
			realtime?: number;
			unfurl?: number;
			lifecycle?: number;
			batch?: number;
		};
	};
	ncmec: {
		enabled: boolean;
		baseUrl?: string;
		username?: string;
		password?: string;
		reporterEmail?: string;
	};
}

export interface BlueskyOAuthKeyConfig {
	kid: string;
	private_key?: string;
	private_key_path?: string;
}

export interface BlueskyOAuthConfig {
	enabled: boolean;
	client_name: string;
	client_uri: string;
	logo_uri: string;
	tos_uri: string;
	policy_uri: string;
	keys: Array<BlueskyOAuthKeyConfig>;
}
