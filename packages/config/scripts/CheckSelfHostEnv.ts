// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {type ConfigObject, isConfigObject} from '@fluxer/config/src/config_loader/ConfigObject';
import {NAMED_FLUXER_ENV_NAMES} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {parse} from 'yaml';

const REPO_ROOT = fileURLToPath(new URL('../../../', import.meta.url));
const SELF_HOSTING = path.join(REPO_ROOT, 'fluxer_rexctl/internal/stack/files');
const CHART = path.join(REPO_ROOT, 'deploy/helm/fluxer');

const HOSTED_ONLY_OPERATOR_PASS = 'the Operator Pass is sold only by the hosted service';

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
	FLUXER_LABS_GUILD_ID: HOSTED_ONLY_OPERATOR_PASS,
	FLUXER_LABS_OPERATOR_ROLE_ID: HOSTED_ONLY_OPERATOR_PASS,
	FLUXER_PAYMENT_DISPUTE_CLOSURE_EFFECTIVE_AT: 'payment dispute closures run only on the hosted service',
	FLUXER_OPERATOR_PASS_SALE_OPENS_AT: HOSTED_ONLY_OPERATOR_PASS,
	FLUXER_OPERATOR_PASS_PRICE_RISES_AT: HOSTED_ONLY_OPERATOR_PASS,
	FLUXER_OPERATOR_PASS_GRACE_MINUTES: HOSTED_ONLY_OPERATOR_PASS,
	FLUXER_OPERATOR_PASS_NOW: 'test and development only',
	...Object.fromEntries(
		[
			'FLUXER_OPERATOR_PASS_RELEASE_LISTS',
			'FLUXER_OPERATOR_PASS_TRANSFER_LOCK_HOURS',
			'FLUXER_OPERATOR_PASS_WITHDRAWAL_COUNTRIES',
			'FLUXER_OPERATOR_PASS_STOPPED_COUNTRIES',
			'FLUXER_OPERATOR_PASS_WALL_TIERS',
			'FLUXER_OPERATOR_PASS_MAX_QUANTITY',
			...['BRL', 'DKK', 'INR', 'NOK', 'PLN', 'SEK', 'TRY'].map(
				(currency) => `FLUXER_OPERATOR_PASS_PRICE_${currency}_ENABLED`,
			),
			...['USD', 'EUR', 'BRL', 'DKK', 'INR', 'NOK', 'PLN', 'SEK', 'TRY'].map(
				(currency) => `FLUXER_OPERATOR_PASS_STANDARD_PRICE_${currency}`,
			),
		].map((name) => [name, HOSTED_ONLY_OPERATOR_PASS]),
	),
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
	MEILI_LOG_LEVEL: 'FLUXER_MEILISEARCH_LOG_LEVEL',
	MALLOC_ARENA_MAX: 'FLUXER_MALLOC_ARENA_MAX',
	TOKIO_WORKER_THREADS: 'FLUXER_TOKIO_WORKER_THREADS',
	WEED_MASTER_VOLUME_GROWTH_COPY_1: 'FLUXER_SEAWEEDFS_VOLUME_GROWTH',
	LIVEKIT_KEYS: 'LIVEKIT_API_KEY',
	NODE_EXTRA_CA_CERTS: 'FLUXER_NODE_EXTRA_CA_CERTS',
	FLUXER_EDGE_TLS_DIRECTIVE: 'FLUXER_EDGE_TLS',
	FLUXER_API_PUBLIC_ENDPOINT: 'FLUXER_API_ENDPOINT',
	http_proxy: 'HTTP_PROXY',
	https_proxy: 'HTTPS_PROXY',
};

const OWN_POOL_SIZES = new Set(['api', 'worker', 'users', 'messages']);
const OBJECT_STORE_CREDENTIALS = ['FLUXER_S3_ACCESS_KEY', 'FLUXER_S3_SECRET_KEY'];
const OBJECT_STORE_CLIENTS = ['api', 'worker', 'media-proxy'];

const problems: Array<string> = [];

function readSelfHosting(name: string): string {
	return readFileSync(path.join(SELF_HOSTING, name), 'utf8');
}

function mapping(value: unknown): ConfigObject {
	return isConfigObject(value) ? value : {};
}

function checkHelmChart(): void {
	const composeCaddyfile = readFileSync(path.join(SELF_HOSTING, 'Caddyfile'));
	const chartCaddyfile = readFileSync(path.join(CHART, 'files/Caddyfile'));
	if (!chartCaddyfile.equals(composeCaddyfile)) {
		problems.push('deploy/helm/fluxer/files/Caddyfile differs from fluxer_rexctl/internal/stack/files/Caddyfile');
	}
}

function expectedInput(serviceName: string, key: string): string {
	const prefix = `FLUXER_${serviceName.toUpperCase().replace('-', '_')}`;
	if (key === 'FLUXER_POSTGRES_MAX_CONNECTIONS' && OWN_POOL_SIZES.has(serviceName)) {
		return `${prefix}_POSTGRES_MAX_CONNECTIONS`;
	}
	if (key === 'GOMEMLIMIT' || (key === 'TOKIO_WORKER_THREADS' && serviceName === 'media-proxy')) {
		return `${prefix}_${key}`;
	}
	return INPUT_NAMES[key] ?? key;
}

function checkComposeForwarding(): void {
	const services = mapping(
		mapping(parse(readSelfHosting('docker-compose.yml'), {merge: true, maxAliasCount: -1})).services,
	);
	const service = (name: string) => mapping(services[name]);
	const environment = (name: string): Record<string, string> =>
		Object.fromEntries(Object.entries(mapping(service(name).environment)).map(([key, value]) => [key, String(value)]));

	const api = environment('api');
	for (const name of NAMED_FLUXER_ENV_NAMES) {
		if (!(name in api) && !(name in API_SETTINGS_NOT_FORWARDED)) {
			problems.push(`the api service does not forward ${name}, which its config loader reads`);
		}
	}
	for (const name of Object.keys(API_SETTINGS_NOT_FORWARDED)) {
		if (!NAMED_FLUXER_ENV_NAMES.includes(name)) {
			problems.push(`${name} is listed as not forwarded but the config loader no longer reads it`);
		}
	}

	for (const serviceName of Object.keys(services)) {
		for (const [key, value] of Object.entries(environment(serviceName))) {
			const match = /^\$\{([A-Z][A-Z0-9_]*)/u.exec(value.trim());
			if (match == null) {
				continue;
			}
			const expected = expectedInput(serviceName, key);
			if (match[1] !== expected) {
				problems.push(`${serviceName}.${key} reads ${match[1]} from .env, expected ${expected}`);
			}
		}
	}

	for (const name of ['FLUXER_REPORT_RETENTION_DAYS', 'FLUXER_REPORT_RETENTION_DRY_RUN']) {
		if (environment('worker')[name] !== `\${${name}:-}`) {
			problems.push(`the worker must forward ${name} as \${${name}:-}`);
		}
	}

	const entrypoint = service('seaweedfs-init').entrypoint;
	const init = Array.isArray(entrypoint) && entrypoint.length === 3 ? String(entrypoint[2]) : '';
	if (!Array.isArray(entrypoint) || entrypoint[0] !== '/bin/sh' || entrypoint[1] !== '-c' || init === '') {
		problems.push('seaweedfs-init must run one script through /bin/sh -c');
	}
	if (
		!init.includes('s3.configure -user=fluxer -access_key=$$FLUXER_S3_ACCESS_KEY -secret_key=$$FLUXER_S3_SECRET_KEY') ||
		!init.includes('-apply')
	) {
		problems.push('seaweedfs-init must apply an S3 identity built from the .env credentials');
	}
	const configure = init.indexOf('s3.configure');
	if (configure === -1 || init.indexOf('echo "buckets ready"') < configure) {
		problems.push('seaweedfs-init must configure the S3 identity before it reports the buckets ready');
	}
	if (!init.includes('could not configure the S3 identity') || !/exit 1;?\s*$/u.test(init)) {
		problems.push('seaweedfs-init must exit 1 when the S3 identity write fails');
	}
	if (!readSelfHosting('docker-compose.yml').includes('${FLUXER_SETTINGS_LOADED:?')) {
		problems.push('docker-compose.yml must require FLUXER_SETTINGS_LOADED');
	}
	for (const name of OBJECT_STORE_CREDENTIALS) {
		if (environment('seaweedfs-init')[name] !== `\${${name}:-}`) {
			problems.push(`seaweedfs-init must forward ${name}`);
		}
	}
	if (environment('media-proxy').FLUXER_S3_READ_SIGNED !== `\${FLUXER_S3_READ_SIGNED:-true}`) {
		problems.push('media-proxy must sign its object store reads by default');
	}
	for (const name of OBJECT_STORE_CLIENTS) {
		const dependency = mapping(mapping(service(name).depends_on)['seaweedfs-init']);
		if (dependency.condition !== 'service_completed_successfully') {
			problems.push(`${name} must wait for seaweedfs-init to complete successfully`);
		}
	}
}

checkHelmChart();
checkComposeForwarding();

if (problems.length > 0) {
	console.error(`self-host env check found ${problems.length} problem(s):`);
	for (const problem of problems) {
		console.error(`  ${problem}`);
	}
	process.exit(1);
}
console.log('self-host env check passed');
