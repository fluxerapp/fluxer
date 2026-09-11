// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import {normalizePublicEndpoint} from '@fluxer/config/src/EndpointDerivation';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

import {serviceEnvironment, serviceList, serviceNames} from './SelfHostingCompose';

const DOMAIN = 'chat.example.com';
const PUBLIC_PORT = '19080';
const ORIGIN_PORT = '29080';

const SECRETS: Record<string, string> = {
	FLUXER_DOMAIN: DOMAIN,
	POSTGRES_PASSWORD: 'postgres-password',
	MEILI_MASTER_KEY: 'meili-master-key',
	FLUXER_S3_ACCESS_KEY: 's3-access-key',
	FLUXER_S3_SECRET_KEY: 's3-secret-key',
	LIVEKIT_API_KEY: 'livekit-api-key',
	LIVEKIT_API_SECRET: 'livekit-api-secret',
	FLUXER_ERLANG_COOKIE: 'erlang-cookie',
	FLUXER_SUDO_MODE_SECRET: 'sudo-mode-secret',
	FLUXER_CONNECTION_INITIATION_SECRET: 'connection-initiation-secret',
	FLUXER_GATEWAY_RPC_AUTH_TOKEN: 'gateway-rpc-auth-token',
	FLUXER_MEDIA_PROXY_SECRET_KEY: 'media-proxy-secret-key',
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: 'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=',
	FLUXER_ADMIN_SECRET_KEY_BASE: 'admin-secret-key-base',
	FLUXER_ADMIN_OAUTH_CLIENT_SECRET: 'admin-oauth-client-secret',
	FLUXER_VAPID_PUBLIC_KEY: 'BB76bTFIuoqmxJtTfZX0yGTn1f_qu9H03B_nkj8OyExJFkN7Y-HBZZzShnHZoEhXKc5ZRy3jFu7OkBbnaQG-4aw',
	FLUXER_VAPID_PRIVATE_KEY: 'Xgi-3P8J-I3Q6U1HlCcXMuc_tKLGAM9nIfznX3Hz68o',
};

const PORT_ONLY_ENV: Record<string, string> = {
	...SECRETS,
	FLUXER_PUBLIC_SCHEME: 'http',
	FLUXER_PUBLIC_PORT: PUBLIC_PORT,
};

const DOCUMENTED_RECIPE_ENV: Record<string, string> = {
	...PORT_ONLY_ENV,
	FLUXER_PUBLIC_ORIGIN: `http://${DOMAIN}:${PUBLIC_PORT}`,
	FLUXER_HTTP_PORT: PUBLIC_PORT,
};

const ORIGIN_ONLY_ENV: Record<string, string> = {
	...SECRETS,
	FLUXER_PUBLIC_ORIGIN: `https://${DOMAIN}:${ORIGIN_PORT}`,
	FLUXER_HTTPS_PORT: ORIGIN_PORT,
};

const SERVICES_WITHOUT_ENDPOINT_REPAIR = new Set(['edge']);

function closingBrace(value: string, open: number): number {
	let depth = 0;
	for (let index = open; index < value.length; index += 1) {
		if (value[index] === '{') {
			depth += 1;
		} else if (value[index] === '}') {
			depth -= 1;
			if (depth === 0) {
				return index;
			}
		}
	}
	throw new Error(`unbalanced interpolation in ${value}`);
}

function expand(value: string, env: Record<string, string>): string {
	let out = '';
	let index = 0;
	while (index < value.length) {
		const dollar = value.indexOf('$', index);
		if (dollar === -1) {
			return out + value.slice(index);
		}
		out += value.slice(index, dollar);
		if (value[dollar + 1] !== '{') {
			const bare = /^[A-Za-z_][A-Za-z0-9_]*/u.exec(value.slice(dollar + 1));
			out += bare ? (env[bare[0]] ?? '') : '$';
			index = dollar + 1 + (bare ? bare[0].length : 0);
			continue;
		}
		const close = closingBrace(value, dollar + 1);
		const inner = value.slice(dollar + 2, close);
		const operator = inner.search(/:[-?+]/u);
		const name = operator === -1 ? inner : inner.slice(0, operator);
		const fallback = operator === -1 ? '' : inner.slice(operator + 2);
		const current = env[name] ?? '';
		if (current.length > 0) {
			out += inner[operator + 1] === '+' && operator !== -1 ? expand(fallback, env) : current;
		} else if (inner[operator + 1] === '-' && operator !== -1) {
			out += expand(fallback, env);
		} else if (inner[operator + 1] === '?' && operator !== -1) {
			throw new Error(`${name} is required: ${fallback}`);
		}
		index = close + 1;
	}
	return out;
}

function expandedEnvironment(service: string, env: Record<string, string>): Record<string, string> {
	const expanded: Record<string, string> = {};
	for (const [key, value] of Object.entries(serviceEnvironment(service))) {
		expanded[key] = expand(value, env);
	}
	return expanded;
}

function publicUrl(value: string): URL | null {
	let parsed: URL;
	try {
		parsed = new URL(value);
	} catch {
		return null;
	}
	return parsed.hostname === DOMAIN ? parsed : null;
}

function publicUrlNames(environment: Record<string, string>): Array<string> {
	return Object.keys(environment)
		.filter((name) => publicUrl(environment[name]) !== null)
		.sort();
}

function repairedPublicUrls(service: string, env: Record<string, string>): Array<[string, string]> {
	const environment = expandedEnvironment(service, env);
	const publicPort = Number.parseInt(environment.FLUXER_PUBLIC_PORT ?? '', 10);
	return publicUrlNames(environment).map((name) => [
		`${service}.${name}`,
		normalizePublicEndpoint(
			environment[name],
			environment.FLUXER_BASE_DOMAIN ?? '',
			Number.isNaN(publicPort) ? undefined : publicPort,
		),
	]);
}

function withoutPort(entries: Array<[string, string]>, port: string): Array<string> {
	return entries
		.filter(([, value]) => publicUrl(value)?.port !== port)
		.map(([name, value]) => `${name}=${value}`)
		.sort();
}

function publishedPorts(env: Record<string, string>): Array<string> {
	return serviceList('edge', 'ports')
		.map((mapped) => expand(mapped, env).split(':'))
		.map((parts) => parts[parts.length - 2] ?? '');
}

function browserFacingUrls(config: MasterConfig): Array<[string, string]> {
	const entries: Array<[string, string]> = [
		...Object.entries(config.endpoints),
		['integrations.voice.url', config.integrations.voice.url],
		['services.media_proxy.upload_relay.endpoint', config.services.media_proxy.upload_relay.endpoint],
		...config.auth.passkeys.additional_allowed_origins.map((origin, index): [string, string] => [
			`auth.passkeys.additional_allowed_origins.${index}`,
			origin,
		]),
	];
	return entries.filter(([, value]) => publicUrl(value) !== null);
}

async function loadApiConfig(env: Record<string, string>): Promise<MasterConfig> {
	for (const key of Object.keys(process.env)) {
		if (key.startsWith('FLUXER_')) {
			vi.stubEnv(key, undefined);
		}
	}
	for (const [key, value] of Object.entries(expandedEnvironment('api', env))) {
		if (key.startsWith('FLUXER_')) {
			vi.stubEnv(key, value);
		}
	}
	return loadConfig();
}

describe('Compose environment interpolation', () => {
	test.each([
		[`\${VALUE}`, {VALUE: 'set'}, 'set'],
		['$VALUE/path', {VALUE: 'set'}, 'set/path'],
		[`\${VALUE:-fallback}`, {VALUE: ''}, 'fallback'],
		[`\${VALUE:-\${OTHER:-fallback}}`, {OTHER: 'nested'}, 'nested'],
		[`\${VALUE:+alternate}`, {VALUE: 'set'}, 'alternate'],
		[`\${VALUE:+alternate}`, {}, ''],
		[`\${VALUE:?required}`, {VALUE: 'set'}, 'set'],
	] as const)('expands %s with %j to %s', (source, environment, expected) => {
		expect(expand(source, environment)).toBe(expected);
	});

	test('rejects a missing required value and unbalanced interpolation', () => {
		expect(() => expand(`\${VALUE:?required}`, {})).toThrow('VALUE is required: required');
		expect(() => expand('${VALUE:-fallback', {})).toThrow('unbalanced interpolation');
	});
});

describe('the shipped compose stack expanded on a non-default port', () => {
	test('every service handed a public URL is handed the base domain and port that repair it', () => {
		const starved = serviceNames
			.filter((service) => !SERVICES_WITHOUT_ENDPOINT_REPAIR.has(service))
			.filter((service) => {
				const environment = expandedEnvironment(service, PORT_ONLY_ENV);
				return (
					publicUrlNames(environment).length > 0 && (!environment.FLUXER_BASE_DOMAIN || !environment.FLUXER_PUBLIC_PORT)
				);
			});
		expect(starved).toEqual([]);
	});

	test('every public URL the stack hands a browser carries the port', () => {
		const entries = serviceNames
			.filter((service) => !SERVICES_WITHOUT_ENDPOINT_REPAIR.has(service))
			.flatMap((service) => repairedPublicUrls(service, PORT_ONLY_ENV));
		expect(entries.length).toBeGreaterThan(0);
		expect(withoutPort(entries, PUBLIC_PORT)).toEqual([]);
	});

	test('the edge listens on its scheme default whatever the public port is', () => {
		const environment = expandedEnvironment('edge', PORT_ONLY_ENV);
		expect(environment.FLUXER_EDGE_SITE_ADDRESS).toBe(`http://${DOMAIN}`);
		expect(publishedPorts(PORT_ONLY_ENV)).not.toContain(PUBLIC_PORT);
	});

	test('the documented recipe publishes the port every public URL advertises', () => {
		expect(publishedPorts(DOCUMENTED_RECIPE_ENV)).toContain(PUBLIC_PORT);
	});
});

describe('a public origin carrying a port while FLUXER_PUBLIC_PORT stays standard', () => {
	beforeEach(() => {
		resetConfig();
	});

	afterEach(() => {
		resetConfig();
		vi.unstubAllEnvs();
	});

	test('the compose overrides all carry the origin port', () => {
		const environment = expandedEnvironment('api', ORIGIN_ONLY_ENV);
		const entries = publicUrlNames(environment).map((name): [string, string] => [name, environment[name]]);
		expect(entries.length).toBeGreaterThan(0);
		expect(withoutPort(entries, ORIGIN_PORT)).toEqual([]);
	});

	test('the loaded config ports every public URL using the explicit origin', async () => {
		const loaded = await loadApiConfig(ORIGIN_ONLY_ENV);
		expect(withoutPort(browserFacingUrls(loaded), ORIGIN_PORT)).toEqual([]);
	});

	test('the port-only recipe ports every public URL the loaded config exposes', async () => {
		const loaded = await loadApiConfig(PORT_ONLY_ENV);
		expect(withoutPort(browserFacingUrls(loaded), PUBLIC_PORT)).toEqual([]);
	});
});
