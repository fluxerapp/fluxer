// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {CachePurgeAdapterName, StoreProductSlotName} from '@fluxer/config/src/MasterConfig';

export type APIWorkerMode = 'all_lanes' | 'single_lane' | 'single_task';
export type APIWorkerLaneName = 'realtime' | 'unfurl' | 'lifecycle' | 'batch';
export type PushProviderEnvironment = 'production' | 'development';

export interface PushProviderAppConfig {
	appId: string;
	topic?: string;
	environment?: PushProviderEnvironment;
}

export interface AppStoreAppConfig {
	bundleId: string;
	appAppleId: number;
}

export interface APICachePurgeConfig {
	adapter: CachePurgeAdapterName;
	http: {
		endpoint: string;
		token: string;
		timeoutMs: number;
	};
}

interface APIGeoipFilesystemConfig {
	mode: 'filesystem';
	maxmindDbPath?: string;
}

interface APIGeoipS3Config {
	mode: 's3';
	maxmindDbPath: string;
	s3Bucket: string;
	s3Key: string;
}

export type APIGeoipConfig = APIGeoipFilesystemConfig | APIGeoipS3Config;

export interface APIConfig {
	nodeEnv: 'development' | 'production';
	port: number;
	headersTimeoutMs: number;
	requestTimeoutMs: number;
	maxInflightRequests: number;
	ipBanExemptIps: Array<string>;
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
		preparedStatements: boolean;
	};
	database: {
		backend: 'cassandra' | 'postgres';
	};
	kv: {
		url: string;
		mode: 'standalone' | 'cluster';
	};
	nats: {
		coreUrl: string;
		jetStreamUrl: string;
		authToken: string;
	};
	storageChangeFeed: {
		enabled: boolean;
		stream: string;
		skipBuckets: Array<string>;
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
		attachmentUrls: {
			secretsBase64: Array<string>;
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
		webAppOrigins: Array<string>;
		gateway: string;
		media: string;
		staticCdn: string;
		marketing: string;
		admin: string;
		invite: string;
		gift: string;
	};
	internal: {
		gatewayRpcAuthToken: string;
		donationProxyKey: string;
	};
	hosts: {
		marketing: string;
		unfurlIgnored: Array<string>;
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
		};
	};
	email: {
		enabled: boolean;
		provider: 'smtp' | 'none';
		webhookSecret?: string;
		fromEmail: string;
		fromName: string;
		appBaseUrl: string;
		smtp?: {
			host: string;
			port: number;
			username: string;
			password: string;
			secure: boolean;
		};
	};
	blocklistFeeds: {
		enabled: boolean;
	};
	breachedPasswordCheck: {
		enabled: boolean;
	};
	voice: {
		enabled: boolean;
		apiKey?: string;
		apiSecret?: string;
		url?: string;
		internalUrl?: string;
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
			monthlyDkk?: string;
			monthlyInr?: string;
			monthlyNok?: string;
			monthlyPln?: string;
			monthlySek?: string;
			monthlyTry?: string;
			yearlyUsd?: string;
			yearlyEur?: string;
			yearlyBrl?: string;
			yearlyDkk?: string;
			yearlyInr?: string;
			yearlyNok?: string;
			yearlyPln?: string;
			yearlySek?: string;
			yearlyTry?: string;
			gift1MonthUsd?: string;
			gift1MonthEur?: string;
			gift1MonthSek?: string;
			gift1YearSek?: string;
			gift1MonthDkk?: string;
			gift1YearDkk?: string;
			gift1MonthNok?: string;
			gift1YearNok?: string;
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
		legacyPrices?: Record<string, Array<string> | undefined>;
	};
	cachePurge: APICachePurgeConfig;
	clamav: {
		enabled: boolean;
		host: string;
		port: number;
		failOpen: boolean;
	};
	admin: {
		oauthClientSecret?: string;
	};
	auth: {
		sudoModeSecret: string;
		connectionInitiationSecret: string;
		ssoAllowPrivateAddresses: boolean;
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
			statusPageUrl?: string;
			statusPageIncidentHistoryUrl?: string;
		};
		setup: {
			configured: boolean;
		};
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
		validateResponses: boolean;
	};
	presignedAttachmentUploadsEnabled: boolean;
	presignedHarvestDownloadsEnabled: boolean;
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
			apps: Array<PushProviderAppConfig>;
		};
	};
	appStore: {
		enabled: boolean;
		issuerId?: string;
		keyId?: string;
		privateKey?: string;
		privateKeyPath?: string;
		apps: Array<AppStoreAppConfig>;
		products: Record<string, StoreProductSlotName>;
	};
	googlePlay: {
		enabled: boolean;
		packages: Array<string>;
		clientEmail?: string;
		privateKey?: string;
		privateKeyPath?: string;
		serviceAccountJsonPath?: string;
		tokenUri: string;
		products: Record<string, StoreProductSlotName>;
		pushAudience?: string;
		pushServiceAccountEmail?: string;
	};
	storeBilling: {
		sandboxUserIds: Array<string>;
		sandboxEntitlesAll: boolean;
	};
	worker: {
		mode: APIWorkerMode;
		laneName?: APIWorkerLaneName;
		taskName?: WorkerTaskName;
		enableCronScheduler?: boolean;
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
