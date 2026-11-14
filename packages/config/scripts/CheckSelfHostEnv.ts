// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {type ConfigObject, isConfigObject} from '@fluxer/config/src/config_loader/ConfigObject';
import {parse} from 'yaml';

const REPO_ROOT = fileURLToPath(new URL('../../../', import.meta.url));
const SELF_HOSTING = path.join(REPO_ROOT, 'fluxer_rexctl/internal/stack/files');
const CHART = path.join(REPO_ROOT, 'deploy/helm/fluxer');

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
const FILESYSTEM_OBJECT_STORE_OVERLAY = 'filesystem-object-store.compose.yml';
const KV_POSTGRES_OVERLAY = 'kv-postgres.compose.yml';
const OBJECTS_VOLUME = 'objects-data';
const MEDIA_PROXY_GROUP = '10001';
const ROOT_USERS = new Set(['0', 'root', '0:0', 'root:root']);
const KV_CLIENTS = ['api', 'worker', 'gateway'];

const problems: Array<string> = [];

function readSelfHosting(name: string): string {
	return readFileSync(path.join(SELF_HOSTING, name), 'utf8');
}

function mapping(value: unknown): ConfigObject {
	return isConfigObject(value) ? value : {};
}

type ComposeServices = (name: string) => ConfigObject;

function composeServices(name: string): {names: Array<string>; service: ComposeServices} | null {
	if (!existsSync(path.join(SELF_HOSTING, name))) {
		return null;
	}
	const document = parse(readSelfHosting(name), {
		merge: true,
		maxAliasCount: -1,
		customTags: [{tag: '!reset', resolve: () => null}],
		logLevel: 'error',
	});
	const services = mapping(mapping(document).services);
	return {names: Object.keys(services), service: (serviceName) => mapping(services[serviceName])};
}

function environmentOf(service: ConfigObject): Record<string, string> {
	return Object.fromEntries(Object.entries(mapping(service.environment)).map(([key, value]) => [key, String(value)]));
}

function inactive(service: ConfigObject): boolean {
	return Array.isArray(service.profiles) && service.profiles.length > 0;
}

function dependency(service: ConfigObject, name: string): unknown {
	return mapping(service.depends_on)[name];
}

function mountTargets(service: ConfigObject, volume: string): Array<string> {
	const volumes = Array.isArray(service.volumes) ? service.volumes : [];
	return volumes.flatMap((entry) => {
		if (typeof entry === 'string') {
			const [source, target] = entry.split(':');
			return source === volume && target ? [target] : [];
		}
		const long = mapping(entry);
		return long.source === volume && typeof long.target === 'string' ? [long.target] : [];
	});
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

function checkForwarding(file: string, names: Array<string>, service: ComposeServices): void {
	for (const serviceName of names) {
		for (const [key, value] of Object.entries(environmentOf(service(serviceName)))) {
			const match = /^\$\{([A-Z][A-Z0-9_]*)/u.exec(value.trim());
			if (match == null) {
				continue;
			}
			const expected = expectedInput(serviceName, key);
			if (match[1] !== expected) {
				problems.push(`${file}: ${serviceName}.${key} reads ${match[1]} from .env, expected ${expected}`);
			}
		}
	}
}

function checkComposeForwarding(): void {
	const compose = composeServices('docker-compose.yml');
	if (compose === null) {
		problems.push('docker-compose.yml is missing');
		return;
	}
	const {service} = compose;
	const environment = (name: string) => environmentOf(service(name));
	checkForwarding('docker-compose.yml', compose.names, service);

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

function checkFilesystemObjectStoreOverlay(): void {
	const file = FILESYSTEM_OBJECT_STORE_OVERLAY;
	const overlay = composeServices(file);
	if (overlay === null) {
		return;
	}
	const {service} = overlay;
	checkForwarding(file, overlay.names, service);
	for (const name of ['seaweedfs', 'seaweedfs-init']) {
		if (!inactive(service(name))) {
			problems.push(`${file} must move ${name} into a profile`);
		}
	}
	const init = service('objects-init');
	if (!overlay.names.includes('objects-init') || inactive(init)) {
		problems.push(`${file} must run objects-init`);
	} else if (!ROOT_USERS.has(String(init.user))) {
		problems.push(`${file}: objects-init must run as root to create the folders`);
	}
	for (const name of OBJECT_STORE_CLIENTS) {
		const client = service(name);
		if (dependency(client, 'seaweedfs-init') !== null) {
			problems.push(`${file}: ${name} must reset its seaweedfs-init dependency`);
		}
		if (mapping(dependency(client, 'objects-init')).condition !== 'service_completed_successfully') {
			problems.push(`${file}: ${name} must wait for objects-init to complete successfully`);
		}
		const rootKey = name === 'media-proxy' ? 'FLUXER_MEDIA_PROXY_STORAGE_ROOT' : 'FLUXER_STORAGE_ROOT';
		const root = environmentOf(client)[rootKey];
		if (root === undefined || !mountTargets(client, OBJECTS_VOLUME).includes(root)) {
			problems.push(`${file}: ${name} must mount ${OBJECTS_VOLUME} at its ${rootKey}`);
		}
	}
	for (const name of ['api', 'worker']) {
		const client = service(name);
		if (environmentOf(client).FLUXER_STORAGE_BACKEND !== 'filesystem') {
			problems.push(`${file}: ${name} must set FLUXER_STORAGE_BACKEND to filesystem`);
		}
		const groups = Array.isArray(client.group_add) ? client.group_add.map(String) : [];
		if (!groups.includes(MEDIA_PROXY_GROUP)) {
			problems.push(`${file}: ${name} must join group ${MEDIA_PROXY_GROUP} to write the files media-proxy owns`);
		}
	}
	if (environmentOf(service('media-proxy')).FLUXER_MEDIA_PROXY_STORAGE_BACKEND !== 'local') {
		problems.push(`${file}: media-proxy must set FLUXER_MEDIA_PROXY_STORAGE_BACKEND to local`);
	}
}

function checkKVPostgresOverlay(): void {
	const file = KV_POSTGRES_OVERLAY;
	const overlay = composeServices(file);
	if (overlay === null) {
		return;
	}
	const {service} = overlay;
	checkForwarding(file, overlay.names, service);
	if (!inactive(service('valkey'))) {
		problems.push(`${file} must move valkey into a profile`);
	}
	for (const name of KV_CLIENTS) {
		if (dependency(service(name), 'valkey') !== null) {
			problems.push(`${file}: ${name} must reset its valkey dependency`);
		}
	}
	for (const name of ['api', 'worker']) {
		if (environmentOf(service(name)).FLUXER_KV_MODE !== 'postgres') {
			problems.push(`${file}: ${name} must set FLUXER_KV_MODE to postgres`);
		}
	}
}

checkHelmChart();
checkComposeForwarding();
checkFilesystemObjectStoreOverlay();
checkKVPostgresOverlay();

if (problems.length > 0) {
	console.error(`self-host env check found ${problems.length} problem(s):`);
	for (const problem of problems) {
		console.error(`  ${problem}`);
	}
	process.exit(1);
}
console.log('self-host env check passed');
