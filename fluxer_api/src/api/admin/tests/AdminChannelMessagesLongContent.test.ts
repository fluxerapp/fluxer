// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {BrowseChannelResponse} from '@fluxer/schema/src/domains/admin/AdminMessageBrowseSchemas';
import type {LimitConfigGetResponse, LookupMessageResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

const LONG_CONTENT_LENGTH = 10_000;

describe('admin message tools with messages above the stock length limit', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let originalSelfHosted: boolean;
	let originalPremiumMode: ReturnType<typeof getCachedInstancePremiumMode>;

	beforeAll(async () => {
		originalSelfHosted = getConfig().instance.selfHosted;
		originalPremiumMode = getCachedInstancePremiumMode();
		getConfig().instance.selfHosted = true;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.MESSAGE_LOOKUP,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
		const {limit_config: config} = await createBuilder<z.infer<typeof LimitConfigGetResponse>>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: config.traitDefinitions,
					rules: config.rules.map(({modifiedFields: _modifiedFields, ...rule}) => ({
						...rule,
						limits: {...rule.limits, max_message_length: 100_000},
					})),
				},
			})
			.execute();
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	});

	it('lists and looks up a channel message longer than the stock limit', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const content = 'a'.repeat(LONG_CONTENT_LENGTH);
		const message = await sendChannelMessage(harness, owner.token, systemChannel.id, content);

		const browse = await createBuilder<z.infer<typeof BrowseChannelResponse>>(harness, admin.token)
			.get(`/admin/channels/${systemChannel.id}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(browse.messages.find((entry) => entry.id === message.id)?.content).toBe(content);

		const lookup = await createBuilder<z.infer<typeof LookupMessageResponse>>(harness, admin.token)
			.get(`/admin/channels/${systemChannel.id}/messages/${message.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(lookup.messages.find((entry) => entry.id === message.id)?.content).toBe(content);
	});
});
