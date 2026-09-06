// SPDX-License-Identifier: AGPL-3.0-or-later

import {Hono} from 'hono';
import {afterEach, describe, expect, it} from 'vitest';
import type {SsoService} from '../auth/services/SsoService';
import {setCassandraQueryExecutorForTesting} from '../database/CassandraQueryExecution';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '../test/mocks/MockKVProvider';
import type {HonoEnv} from '../types/HonoEnv';
import {InstanceConfigRepository} from './InstanceConfigRepository';
import {InstanceController} from './InstanceController';

interface DiscoveryCaptcha {
	provider: string;
	hcaptcha_site_key: string | null;
	turnstile_site_key: string | null;
}

describe('InstanceController discovery captcha', () => {
	const repositories: Array<InstanceConfigRepository> = [];

	afterEach(() => {
		for (const repository of repositories) {
			repository.shutdown();
		}
		repositories.length = 0;
	});

	function createRepository(): InstanceConfigRepository {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		const repository = new InstanceConfigRepository(new MockKVProvider());
		repositories.push(repository);
		return repository;
	}

	function createApp(repository: InstanceConfigRepository): Hono<HonoEnv> {
		const app = new Hono<HonoEnv>({strict: true});
		app.use('*', async (ctx, next) => {
			ctx.set('instanceConfigRepository', repository);
			ctx.set('limitConfigService', {
				getConfigWireFormat: () => ({version: 2, traitDefinitions: [], rules: [], defaultsHash: 'test'}),
			} as unknown as LimitConfigService);
			ctx.set('ssoService', {
				getPublicStatus: async () => ({
					enabled: false,
					enforced: false,
					display_name: null,
					redirect_uri: '',
				}),
			} as unknown as SsoService);
			await next();
		});
		InstanceController(app);
		return app;
	}

	async function readCaptcha(repository: InstanceConfigRepository): Promise<DiscoveryCaptcha> {
		const response = await createApp(repository).request('http://localhost/.well-known/fluxer');
		expect(response.status).toBe(200);
		return ((await response.json()) as {captcha: DiscoveryCaptcha}).captcha;
	}

	it('advertises no provider and no site key while the selected pair is incomplete', async () => {
		const repository = createRepository();
		await repository.setInstanceIntegrationsConfig({
			captcha: {
				provider: 'turnstile',
				hcaptcha_site_key: 'hcaptcha-site-key',
				hcaptcha_secret_key: 'hcaptcha-secret-key',
			},
		});

		await expect(readCaptcha(repository)).resolves.toEqual({
			provider: 'none',
			hcaptcha_site_key: null,
			turnstile_site_key: null,
		});
	});

	it('advertises only the site key that matches the named provider', async () => {
		const repository = createRepository();
		await repository.setInstanceIntegrationsConfig({
			captcha: {
				provider: 'turnstile',
				hcaptcha_site_key: 'hcaptcha-site-key',
				hcaptcha_secret_key: 'hcaptcha-secret-key',
				turnstile_site_key: 'turnstile-site-key',
				turnstile_secret_key: 'turnstile-secret-key',
			},
		});

		await expect(readCaptcha(repository)).resolves.toEqual({
			provider: 'turnstile',
			hcaptcha_site_key: null,
			turnstile_site_key: 'turnstile-site-key',
		});
	});
});
