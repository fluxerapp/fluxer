// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('discovery fields for self-hosted clients', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('keeps friend requests off on an instance that turned direct messages off before the split', async () => {
		await getInstanceConfigRepository().setConfig(
			'instance_policy_config',
			JSON.stringify({direct_messages_disabled: true}),
		);
		const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.community).toMatchObject({direct_messages_disabled: true, friends_disabled: true});
	});

	it('fills friends_disabled from the direct message flag for an older server', () => {
		const document = parseInstanceDiscoveryDocument({
			api_code_version: 1,
			endpoints: {api: 'https://chat.example.com/api', gateway: 'wss://chat.example.com/gateway'},
			community: {single_community: false, single_community_guild_id: null, direct_messages_disabled: true},
		});
		expect(document.community?.friends_disabled).toBe(true);
		expect(document.notice).toBeNull();
	});
});
