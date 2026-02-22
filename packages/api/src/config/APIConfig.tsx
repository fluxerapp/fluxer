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

export interface APIConfig {
	nodeEnv: 'development' | 'production';
	port: number;

	cassandra: {
		hosts: string;
		keyspace: string;
		localDc: string;
		username: string;
		password: string;
	};

	database: {
		backend: 'cassandra' | 'sqlite';
		sqlitePath: string;
	};

	kv: {
		url: string;
		mode: 'standalone' | 'cluster';
		clusterNodes: Array<{host: string; port: number}>;
		clusterNatMap: Record<string, {host: string; port: number}>;
	};

	nats: {
		coreUrl: string;
		jetStreamUrl: string;
		authToken: string;
	};

	mediaProxy: {
		host: string;
		port: number;
		secretKey: string;
	};

	geoip: {
		maxmindDbPath?: string;
	};

	proxy: {
		trust_cf_connecting_ip: boolean;
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

	hosts: {
		invite: string;
		gift: string;
		marketing: string;
		unfurlIgnored: Array<string>;
	};

	s3: {
		endpoint: string;
		presignedUrlBase: string | undefined;
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

	appPublic: {
		sentryDsn: string;
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

	search: {
		engine: 'meilisearch' | 'elasticsearch';
		url: string;
		apiKey: string;
		username: string;
		password: string;
	};

	stripe: {
		enabled: boolean;
		secretKey?: string;
		webhookSecret?: string;
		prices?: {
			monthlyUsd?: string;
			monthlyEur?: string;
			yearlyUsd?: string;
			yearlyEur?: string;
			gift1MonthUsd?: string;
			gift1MonthEur?: string;
			gift1YearUsd?: string;
			gift1YearEur?: string;
		};
	};

	cloudflare: {
		purgeEnabled: boolean;
		zoneId?: string;
		apiToken?: string;
	};

	alerts: {
		webhookUrl?: string;
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

	gif: {
		provider: 'klipy' | 'tenor';
	};

	klipy: {
		apiKey?: string;
	};

	tenor: {
		apiKey?: string;
	};

	youtube: {
		apiKey?: string;
	};

	instance: {
		selfHosted: boolean;
		autoJoinInviteCode?: string;
		visionariesGuildId?: string;
		operatorsGuildId?: string;
		privateKeyPath?: string;
	};

	domain: {
		baseDomain: string;
	};

	federation?: {
		enabled: boolean;
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

	attachmentDecayEnabled: boolean;

	deletionGracePeriodHours: number;
	inactivityDeletionThresholdDays?: number;

	push: {
		publicVapidKey?: string;
	};

	queue: {
		baseUrl: string;
		authSecret?: string;
	};

	photoDna: {
		enabled: boolean;
		hashService: {
			url?: string;
			timeoutMs: number;
		};
		api: {
			endpoint: string;
			subscriptionKey?: string;
			enhance: boolean;
		};
		rateLimit: {
			requestsPerSecond: number;
		};
	};

	csamIntegration: {
		enabled: boolean;
		provider: 'photo_dna' | 'arachnid_shield';
		photoDna: {
			hashServiceUrl: string;
			hashServiceTimeoutMs: number;
			matchEndpoint: string;
			subscriptionKey: string;
			matchEnhance: boolean;
			rateLimitRps: number;
		};
		arachnidShield: {
			endpoint: string;
			username: string;
			password: string;
			timeoutMs: number;
			maxRetries: number;
			retryBackoffMs: number;
		};
	};

	csam: {
		evidenceRetentionDays: number;
		jobRetentionDays: number;
		cleanupBatchSize: number;
		queue: {
			timeoutMs: number;
			maxEntriesPerBatch: number;
			consumerLockTtlSeconds: number;
		};
	};

	ncmec: {
		enabled: boolean;
		baseUrl?: string;
		username?: string;
		password?: string;
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
