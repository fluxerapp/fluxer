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

import {mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {getConfig, loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import {type ConfigObject, deepMerge} from '@fluxer/config/src/config_loader/ConfigObjectMerge';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

function createTempConfig(config: Record<string, unknown>): string {
	const dir = mkdtempSync(path.join(tmpdir(), 'fluxer-config-test-'));
	const configPath = path.join(dir, 'config.json');
	writeFileSync(configPath, JSON.stringify(config));
	return configPath;
}

function makeMinimalConfig(overrides: Record<string, unknown> = {}): Record<string, unknown> {
	const base: ConfigObject = {
		env: 'test',
		domain: {
			base_domain: 'localhost',
			public_port: 8080,
		},
		database: {
			backend: 'sqlite',
			sqlite_path: ':memory:',
		},
		s3: {
			access_key_id: 'test-key',
			secret_access_key: 'test-secret',
			endpoint: 'http://localhost:9000',
		},
		services: {
			server: {port: 8772, host: '0.0.0.0'},
			media_proxy: {secret_key: '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'},
			admin: {
				secret_key_base: 'abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789',
				oauth_client_secret: 'fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210',
			},
			app_proxy: {port: 8773},
			marketing: {
				enabled: false,
				port: 8774,
				host: '0.0.0.0',
				secret_key_base: 'marketing0123456789abcdef0123456789abcdef0123456789abcdef01234567',
			},
			gateway: {
				port: 8771,
				admin_reload_secret: 'deadbeef0123456789abcdef0123456789abcdef0123456789abcdef01234567',
				media_proxy_endpoint: 'http://localhost:8772/media',
			},
		},
		auth: {
			sudo_mode_secret: 'sudo-test-secret',
			connection_initiation_secret: 'connection-initiation-test-secret',
			vapid: {
				public_key: 'test-vapid-public-key',
				private_key: 'test-vapid-private-key',
			},
		},
		integrations: {
			search: {
				url: 'http://127.0.0.1:7700',
				api_key: 'test-search-key',
			},
		},
	};

	return deepMerge(base, overrides as ConfigObject);
}

describe('ConfigLoader', () => {
	let tempPaths: Array<string> = [];

	beforeEach(() => {
		resetConfig();
	});

	afterEach(() => {
		resetConfig();
		for (const p of tempPaths) {
			try {
				rmSync(path.dirname(p), {recursive: true, force: true});
			} catch {}
		}
		tempPaths = [];
	});

	test('loadConfig loads and caches config', async () => {
		const configPath = createTempConfig(makeMinimalConfig());
		tempPaths.push(configPath);

		const config = await loadConfig([configPath]);
		expect(config.env).toBe('test');
		expect(config.domain.base_domain).toBe('localhost');
	});

	test('getConfig throws when config is not loaded', () => {
		expect(() => getConfig()).toThrow('Config not loaded');
	});

	test('resetConfig clears the cache', async () => {
		const configPath = createTempConfig(makeMinimalConfig());
		tempPaths.push(configPath);

		await loadConfig([configPath]);
		expect(() => getConfig()).not.toThrow();

		resetConfig();
		expect(() => getConfig()).toThrow('Config not loaded');
	});

	test('throws when no config file is found', async () => {
		await expect(loadConfig(['/nonexistent/path.json'])).rejects.toThrow('No config file found');
	});

	test('throws when config paths array is empty', async () => {
		await expect(loadConfig([])).rejects.toThrow('FLUXER_CONFIG must be set');
	});

	test('derives endpoints from domain config', async () => {
		const configPath = createTempConfig(makeMinimalConfig());
		tempPaths.push(configPath);

		const config = await loadConfig([configPath]);
		expect(config.endpoints.api).toContain('localhost');
		expect(config.endpoints.api).toContain('/api');
		expect(config.endpoints.gateway).toContain('ws');
	});

	test('endpoint_overrides take precedence over derived endpoints', async () => {
		const configPath = createTempConfig(
			makeMinimalConfig({
				endpoint_overrides: {
					api: 'https://custom-api.example.com',
					api_client: 'https://custom-api-client.example.com',
					gateway: 'wss://custom-gw.example.com',
				},
			}),
		);
		tempPaths.push(configPath);

		const config = await loadConfig([configPath]);
		expect(config.endpoints.api).toBe('https://custom-api.example.com');
		expect(config.endpoints.api_client).toBe('https://custom-api-client.example.com');
		expect(config.endpoints.gateway).toBe('wss://custom-gw.example.com');
		expect(config.endpoints.app).toContain('localhost');
	});

	test('allows unknown properties (but warns)', async () => {
		const warnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
		const configPath = createTempConfig(
			makeMinimalConfig({
				extra_root: true,
				auth: {
					sudo_mode_secret: 'sudo-test-secret',
					connection_initiation_secret: 'connection-initiation-test-secret',
					vapid: {
						public_key: 'test-vapid-public-key',
						private_key: 'test-vapid-private-key',
					},
					extra_auth: 'oops',
				},
			}),
		);
		tempPaths.push(configPath);

		await expect(loadConfig([configPath])).resolves.toBeDefined();
		expect(warnSpy).toHaveBeenCalledWith(expect.stringContaining('"extra_root"'));
		expect(warnSpy).toHaveBeenCalledWith(expect.stringContaining('"extra_auth"'));

		warnSpy.mockRestore();
	});

	test('allows voice enabled without global credentials when default_region is omitted', async () => {
		const configPath = createTempConfig(
			makeMinimalConfig({
				integrations: {
					voice: {
						enabled: true,
					},
				},
			}),
		);
		tempPaths.push(configPath);

		const config = await loadConfig([configPath]);
		expect(config.integrations.voice.enabled).toBe(true);
		expect(config.integrations.voice.api_key).toBeUndefined();
		expect(config.integrations.voice.api_secret).toBeUndefined();
	});

	test('requires voice credentials when default_region bootstrap is configured', async () => {
		const configPath = createTempConfig(
			makeMinimalConfig({
				integrations: {
					voice: {
						enabled: true,
						default_region: {
							id: 'default',
							name: 'Default',
							emoji: ':earth_africa:',
							latitude: 0,
							longitude: 0,
						},
					},
				},
			}),
		);
		tempPaths.push(configPath);

		await expect(loadConfig([configPath])).rejects.toThrow('api_key');
		await expect(loadConfig([configPath])).rejects.toThrow('api_secret');
	});
});
