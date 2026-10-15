// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getInstanceProductName, setCachedProductName} from '@app/api/instance/ProductName';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const WEB_APP_HOST = 'chat.example.org';

describe('getInstanceProductName', () => {
	const config = getConfig();
	const original = {
		selfHosted: config.instance.selfHosted,
		productName: config.instance.branding.productName,
		webApp: config.endpoints.webApp,
	};

	beforeEach(() => {
		config.endpoints.webApp = `https://${WEB_APP_HOST}:8443/app`;
		config.instance.branding.productName = '';
	});

	afterEach(() => {
		config.instance.selfHosted = original.selfHosted;
		config.instance.branding.productName = original.productName;
		config.endpoints.webApp = original.webApp;
		setCachedProductName(null);
	});

	describe('hosted', () => {
		beforeEach(() => {
			config.instance.selfHosted = false;
		});

		it.each([
			[null, 'Fluxer'],
			['Fluxer', 'Fluxer'],
			['Example Chat', 'Example Chat'],
		])('resolves %j to %j', (cached, expected) => {
			setCachedProductName(cached);
			expect(getInstanceProductName()).toBe(expected);
		});
	});

	describe('self-hosted', () => {
		beforeEach(() => {
			config.instance.selfHosted = true;
		});

		it.each([null, '', '  ', 'Fluxer', ' fluxer ', 'FLUXER'])('resolves %j to the web app host', (cached) => {
			setCachedProductName(cached);
			expect(getInstanceProductName()).toBe(WEB_APP_HOST);
		});

		it('resolves a configured default of Fluxer to the web app host', () => {
			config.instance.branding.productName = 'Fluxer';
			expect(getInstanceProductName()).toBe(WEB_APP_HOST);
		});

		it.each(['Fluxer Community', 'Example Chat'])('leaves %j alone', (cached) => {
			setCachedProductName(cached);
			expect(getInstanceProductName()).toBe(cached);
		});

		it('resolves a persisted Fluxer name to the web app host', async () => {
			setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
			const kvProvider = new MockKVProvider();
			const writer = new InstanceConfigRepository(kvProvider);
			const reader = new InstanceConfigRepository(kvProvider);
			try {
				await writer.setConfig('app_public_config', JSON.stringify({branding: {product_name: 'Fluxer'}}));
				setCachedProductName('Example Chat');
				await reader.initialize();
				expect((await reader.getAppPublicConfig()).branding.product_name).toBe('Fluxer');
				expect(getInstanceProductName()).toBe(WEB_APP_HOST);
			} finally {
				writer.shutdown();
				reader.shutdown();
			}
		});
	});
});
