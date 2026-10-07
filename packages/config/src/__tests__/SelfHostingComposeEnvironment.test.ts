// SPDX-License-Identifier: AGPL-3.0-or-later

import {serviceEnvironment, serviceNames} from '@fluxer/config/src/__tests__/SelfHostingCompose';
import {NAMED_FLUXER_ENV_NAMES} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {describe, expect, test} from 'vitest';

const API_SETTINGS_NOT_FORWARDED: Record<string, string> = {
	FLUXER_CASSANDRA_HOSTS: 'the stack runs Postgres only',
	FLUXER_CASSANDRA_PORT: 'the stack runs Postgres only',
	FLUXER_CASSANDRA_KEYSPACE: 'the stack runs Postgres only',
	FLUXER_CASSANDRA_LOCAL_DC: 'the stack runs Postgres only',
	FLUXER_CASSANDRA_USERNAME: 'the stack runs Postgres only',
	FLUXER_CASSANDRA_PASSWORD: 'the stack runs Postgres only',
	FLUXER_API_WORKER_MODE: 'the one worker container runs every lane',
	FLUXER_API_WORKER_LANE: 'the one worker container runs every lane',
	FLUXER_API_WORKER_TASK: 'the one worker container runs every lane',
	FLUXER_API_WORKER_ENABLE_CRON_SCHEDULER: 'the one worker container hosts cron',
	FLUXER_API_WORKER_METRICS_PORT: 'the stack scrapes no worker metrics',
	FLUXER_RELAX_REGISTRATION_RATE_LIMITS: 'test and development only',
	FLUXER_DISABLE_RATE_LIMITS: 'test and development only',
	FLUXER_TEST_MODE_ENABLED: 'test and development only',
	FLUXER_TEST_HARNESS_TOKEN: 'test and development only',
	FLUXER_VALIDATE_RESPONSES: 'test and development only',
	FLUXER_API_AUTOMATED_MESSAGE_DELETION_DELAY_DAYS: 'automated account actions run only on the hosted service',
	...Object.fromEntries(
		['MONTHLY', 'YEARLY', 'GIFT_1_MONTH', 'GIFT_1_YEAR'].flatMap((slot) =>
			['USD', 'EUR', 'BRL', 'DKK', 'INR', 'ISK', 'NOK', 'PLN', 'SEK', 'TRY'].map((currency) => [
				`FLUXER_STRIPE_PRICE_${slot}_${currency}`,
				'FLUXER_STRIPE_PRICES or the dashboard sets prices',
			]),
		),
	),
	...Object.fromEntries(
		NAMED_FLUXER_ENV_NAMES.filter((name) =>
			['FLUXER_APP_STORE_', 'FLUXER_GOOGLE_PLAY_', 'FLUXER_STORE_BILLING_'].some((prefix) => name.startsWith(prefix)),
		).map((name) => [name, 'in-app purchases are sold only by the hosted service']),
	),
};

const INPUT_NAMES: Record<string, string> = {
	FLUXER_BASE_DOMAIN: 'FLUXER_DOMAIN',
	FLUXER_POSTGRES_PASSWORD: 'POSTGRES_PASSWORD',
	FLUXER_SEARCH_API_KEY: 'MEILI_MASTER_KEY',
	FLUXER_S3_ACCESS_KEY_ID: 'FLUXER_S3_ACCESS_KEY',
	FLUXER_S3_SECRET_ACCESS_KEY: 'FLUXER_S3_SECRET_KEY',
	FLUXER_LIVEKIT_API_KEY: 'LIVEKIT_API_KEY',
	FLUXER_LIVEKIT_API_SECRET: 'LIVEKIT_API_SECRET',
	POSTGRES_DB: 'FLUXER_POSTGRES_DATABASE',
	POSTGRES_USER: 'FLUXER_POSTGRES_USERNAME',
	MEILI_ENV: 'FLUXER_MEILISEARCH_ENV',
	MEILI_NO_ANALYTICS: 'FLUXER_MEILISEARCH_NO_ANALYTICS',
	MEILI_MAX_INDEXING_MEMORY: 'FLUXER_MEILISEARCH_MAX_INDEXING_MEMORY',
	MEILI_MAX_INDEXING_THREADS: 'FLUXER_MEILISEARCH_MAX_INDEXING_THREADS',
	GOMEMLIMIT: 'FLUXER_SEAWEEDFS_GOMEMLIMIT',
	WEED_MASTER_VOLUME_GROWTH_COPY_1: 'FLUXER_SEAWEEDFS_VOLUME_GROWTH',
	LIVEKIT_KEYS: 'LIVEKIT_API_KEY',
	NODE_EXTRA_CA_CERTS: 'FLUXER_NODE_EXTRA_CA_CERTS',
};

const OWN_POOL_SIZES = new Set(['api', 'worker', 'users-shard', 'messages-shard']);

const INTERPOLATION = /^\$\{([A-Z][A-Z0-9_]*)/u;

function inputName(service: string, key: string): string {
	if (key === 'FLUXER_POSTGRES_MAX_CONNECTIONS' && OWN_POOL_SIZES.has(service)) {
		return `FLUXER_${service.toUpperCase().replace('-', '_')}_POSTGRES_MAX_CONNECTIONS`;
	}
	return INPUT_NAMES[key] ?? key;
}

function forwardedEntries(): Array<{service: string; key: string; name: string}> {
	return serviceNames.flatMap((service) =>
		Object.entries(serviceEnvironment(service)).flatMap(([key, value]) => {
			const match = INTERPOLATION.exec(value.trim());
			if (match == null) {
				return [];
			}
			return [{service, key, name: match[1]}];
		}),
	);
}

describe('the shipped compose stack forwards settings from .env', () => {
	test('the api is handed every setting its config loader reads', () => {
		const api = serviceEnvironment('api');
		const missing = NAMED_FLUXER_ENV_NAMES.filter((name) => !(name in api) && !(name in API_SETTINGS_NOT_FORWARDED));
		expect(missing).toEqual([]);
		expect(Object.keys(API_SETTINGS_NOT_FORWARDED).filter((name) => !NAMED_FLUXER_ENV_NAMES.includes(name))).toEqual(
			[],
		);
	});

	test('every forwarded setting is read from .env under the name the operator sets', () => {
		const renamed = forwardedEntries()
			.filter(({service, key, name}) => name !== inputName(service, key))
			.map(({service, key, name}) => `${service}.${key} reads ${name}`);
		expect(renamed).toEqual([]);
	});

	test('the worker is handed the report retention switch and gets no value when .env leaves it out', () => {
		const name = 'FLUXER_REPORT_RETENTION_DRY_RUN';
		expect(serviceEnvironment('worker')[name]).toBe(`\${${name}:-}`);
	});
});
