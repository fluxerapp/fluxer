// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateKeyPairSync} from 'node:crypto';
import {existsSync} from 'node:fs';
import type {UserID} from '@app/api/BrandedTypes';
import {buildAPIConfigFromMaster, initializeConfig} from '@app/api/Config';
import {setInjectedMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import type {APIConfig} from '@app/api/config/APIConfig';
import {
	resetCassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
	shutdownCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import {pinChannelThreadsConfigForTesting} from '@app/api/experiment/ChannelThreadsGate';
import type {IUsersServiceClient} from '@app/api/infrastructure/UsersServiceClient';
import {setInjectedUsersServiceClient} from '@app/api/infrastructure/UsersServiceClient';
import {initializeLogger} from '@app/api/Logger';
import {setInjectedKVProvider, setInjectedSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {drainSearchTasks, enableSearchTaskTracking} from '@app/api/search/SearchTaskTracker';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {MockSnowflakeService} from '@app/api/test/mocks/MockSnowflakeService';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {RepositoryBackedMessageResponseDataService} from '@app/api/test/mocks/RepositoryBackedMessageResponseDataService';
import {server} from '@app/api/test/msw/server';
import {mapUserToPartialResponse} from '@app/api/user/UserMappers';
import {loadConfig} from '@fluxer/config/src/ConfigLoader';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, afterEach, beforeAll} from 'vitest';

function defaultNatsUrl(): string {
	return `nats://${existsSync('/.dockerenv') ? 'nats' : '127.0.0.1'}:4222`;
}

function setDefaultTestEnv(): void {
	// API tests use a non-self-hosted baseline; self-hosted scenarios override the loaded config explicitly.
	process.env.FLUXER_SELF_HOSTED = 'false';

	const natsUrl = defaultNatsUrl();
	const defaults: Record<string, string> = {
		FLUXER_ENV: 'test',
		FLUXER_BASE_DOMAIN: 'localhost',
		FLUXER_PUBLIC_SCHEME: 'http',
		FLUXER_PUBLIC_PORT: '8088',
		FLUXER_TRUST_CLIENT_IP_HEADER: 'true',
		FLUXER_CLIENT_IP_HEADER_NAME: 'x-forwarded-for',
		FLUXER_CASSANDRA_HOSTS: '127.0.0.1',
		FLUXER_CASSANDRA_PORT: '9042',
		FLUXER_CASSANDRA_KEYSPACE: 'fluxer_test',
		FLUXER_CASSANDRA_LOCAL_DC: 'datacenter1',
		FLUXER_CASSANDRA_USERNAME: 'cassandra',
		FLUXER_CASSANDRA_PASSWORD: 'cassandra',
		FLUXER_KV_URL: 'redis://127.0.0.1:6379/0',
		FLUXER_NATS_URL: natsUrl,
		FLUXER_NATS_CORE_URL: natsUrl,
		FLUXER_NATS_JETSTREAM_URL: natsUrl,
		FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT: 'http://127.0.0.1:8088/media',
		FLUXER_S3_ENDPOINT: 'http://127.0.0.1:3900',
		FLUXER_S3_REGION: 'local',
		FLUXER_S3_ACCESS_KEY_ID: 'test',
		FLUXER_S3_SECRET_ACCESS_KEY: 'test',
		FLUXER_API_PRESIGNED_ATTACHMENT_UPLOADS_ENABLED: 'false',
		FLUXER_MEDIA_PROXY_SECRET_KEY: 'test-media-secret',
		FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64: 'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=',
		FLUXER_ADMIN_SECRET_KEY_BASE: 'test-admin-secret',
		FLUXER_ADMIN_OAUTH_CLIENT_SECRET: 'test-admin-oauth-secret',
		FLUXER_GATEWAY_RPC_AUTH_TOKEN: 'test-gateway-rpc-token',
		FLUXER_SUDO_MODE_SECRET: 'test-sudo-secret',
		FLUXER_CONNECTION_INITIATION_SECRET: 'test-connection-secret',
		FLUXER_VAPID_PUBLIC_KEY: 'BB76bTFIuoqmxJtTfZX0yGTn1f_qu9H03B_nkj8OyExJFkN7Y-HBZZzShnHZoEhXKc5ZRy3jFu7OkBbnaQG-4aw',
		FLUXER_VAPID_PRIVATE_KEY: 'Xgi-3P8J-I3Q6U1HlCcXMuc_tKLGAM9nIfznX3Hz68o',
		FLUXER_VAPID_EMAIL: 'test@example.com',
		FLUXER_PASSKEY_RP_NAME: 'Fluxer Test',
		FLUXER_PASSKEY_RP_ID: 'localhost',
		FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: 'http://localhost',
		FLUXER_EMAIL_ENABLED: 'true',
		FLUXER_EMAIL_PROVIDER: 'smtp',
		FLUXER_EMAIL_FROM_EMAIL: 'noreply@example.com',
		FLUXER_EMAIL_SMTP_HOST: 'localhost',
		FLUXER_EMAIL_SMTP_PORT: '1025',
		FLUXER_EMAIL_SMTP_USERNAME: 'test',
		FLUXER_EMAIL_SMTP_PASSWORD: 'test',
		FLUXER_EMAIL_SMTP_SECURE: 'false',
		FLUXER_LIVEKIT_ENABLED: 'false',
		FLUXER_STRIPE_ENABLED: 'true',
		FLUXER_SEARCH_ENGINE: 'elasticsearch',
		FLUXER_SEARCH_URL: 'http://127.0.0.1:9200',
		FLUXER_SEARCH_API_KEY: 'test',
		FLUXER_DISCOVERY_ENABLED: 'true',
		FLUXER_RELAX_REGISTRATION_RATE_LIMITS: 'true',
		FLUXER_DISABLE_RATE_LIMITS: 'true',
		FLUXER_TEST_MODE_ENABLED: 'true',
	};
	for (const [key, value] of Object.entries(defaults)) {
		process.env[key] ??= value;
	}
}

class RepositoryBackedUsersServiceClient implements IUsersServiceClient {
	async getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		const userRepository = getUserRepository();
		const result = new Map<UserID, UserPartialResponse>();
		for (const userId of userIds) {
			const user = await userRepository.findUnique(userId);
			if (user) {
				result.set(userId, mapUserToPartialResponse(user));
			}
		}
		return result;
	}

	async invalidateUserCache(_userId: UserID): Promise<void> {}
}

setDefaultTestEnv();
process.env.FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 ??= 'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=';

function generateTestPrivateKeyPem(type: 'ec' | 'rsa'): string {
	const {privateKey} =
		type === 'ec'
			? generateKeyPairSync('ec', {namedCurve: 'prime256v1'})
			: generateKeyPairSync('rsa', {modulusLength: 2048});
	return privateKey.export({format: 'pem', type: 'pkcs8'}).toString();
}

const master = await loadConfig();
const apiConfig = buildAPIConfigFromMaster(master);
const testApiConfig: APIConfig = {
	...apiConfig,
	auth: {
		...apiConfig.auth,
		passkeys: {
			...apiConfig.auth.passkeys,
			rpId: 'localhost',
			allowedOrigins: ['http://localhost'],
		},
	},
	voice: {
		...apiConfig.voice,
		enabled: false,
	},
	stripe: {
		...apiConfig.stripe,
		enabled: true,
		secretKey: 'sk_test_fluxer',
		webhookSecret: 'whsec_test_fluxer',
	},
	appStore: {
		...apiConfig.appStore,
		enabled: true,
		issuerId: '57246542-96fe-1a63-e053-0824d011072a',
		keyId: 'FLUXERTEST',
		privateKey: generateTestPrivateKeyPem('ec'),
		privateKeyPath: undefined,
		apps: [{bundleId: 'com.fluxer', appAppleId: 1234567890}],
		products: {
			'com.fluxer.plutonium.monthly': 'monthly',
			'com.fluxer.plutonium.yearly': 'yearly',
			'com.fluxer.gift.1month': 'gift_1_month',
			'com.fluxer.gift.1year': 'gift_1_year',
		},
	},
	googlePlay: {
		...apiConfig.googlePlay,
		enabled: true,
		packages: ['com.fluxer'],
		clientEmail: 'play-billing@fluxer-test.iam.gserviceaccount.com',
		privateKey: generateTestPrivateKeyPem('rsa'),
		privateKeyPath: undefined,
		serviceAccountJsonPath: undefined,
		tokenUri: 'https://oauth2.googleapis.com/token',
		products: {
			'plutonium:monthly': 'monthly',
			'plutonium:yearly': 'yearly',
			gift_1_month: 'gift_1_month',
			gift_1_year: 'gift_1_year',
		},
		pushAudience: 'https://api.fluxer.test/webhooks/google-play',
		pushServiceAccountEmail: 'rtdn@fluxer-test.iam.gserviceaccount.com',
	},
	storeBilling: {
		sandboxUserIds: [],
		sandboxEntitlesAll: false,
	},
};
testApiConfig.dev.relaxRegistrationRateLimits = true;
testApiConfig.dev.disableRateLimits = true;
testApiConfig.dev.testModeEnabled = true;

initializeConfig(testApiConfig);

const bootstrapLogger = new NoopLogger();

initializeLogger(bootstrapLogger);

setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());

setInjectedKVProvider(new MockKVProvider());
setInjectedSnowflakeService(new MockSnowflakeService({startTimestampMs: Date.now()}));
setInjectedUsersServiceClient(new RepositoryBackedUsersServiceClient());
setInjectedMessageResponseDataService(new RepositoryBackedMessageResponseDataService());

enableSearchTaskTracking();

beforeAll(async () => {
	server.listen({
		onUnhandledRequest: 'error',
	});
});

afterEach(async () => {
	await drainSearchTasks();
	resetCassandraQueryExecutorForTesting();
	getInstanceConfigRepository().clearCacheForTesting();
	server.resetHandlers();
});

afterAll(async () => {
	pinChannelThreadsConfigForTesting(null);
	server.close();
	await shutdownCassandraQueryExecutorForTesting();
});
