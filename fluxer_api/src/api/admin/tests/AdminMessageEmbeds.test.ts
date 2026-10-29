// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {LookupMessageResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';
import type {z} from 'zod';

describe('Admin message embeds', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('returns embeds and webhook details for a webhook message with only embeds', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Admin Embeds Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Embed Hook');
		const message = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({
				username: 'Status Bot',
				embeds: [
					{
						title: 'Deploy finished',
						description: 'Build 42 is live',
						url: 'https://example.com/builds/42',
						fields: [{name: 'Region', value: 'eu-north', inline: true}],
						footer: {text: 'CI pipeline'},
						image: {url: 'https://example.com/image.png'},
						thumbnail: {url: 'https://example.com/thumb.png'},
					},
				],
			})
			.expect(200)
			.execute();
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.MESSAGE_LOOKUP,
		]);
		const result = await createBuilder<z.infer<typeof LookupMessageResponse>>(harness, admin.token)
			.get(`/admin/channels/${channelId}/messages/${message.id}?context_limit=1`)
			.expect(200)
			.execute();
		const adminMessage = result.messages.find((entry) => entry.id === message.id);
		expect(adminMessage).toBeDefined();
		expect(adminMessage!.content).toBe('');
		const embed = adminMessage!.embeds?.[0];
		expect(embed?.title).toBe('Deploy finished');
		expect(embed?.description).toBe('Build 42 is live');
		expect(embed?.url).toBe('https://example.com/builds/42');
		expect(embed?.fields?.[0]).toMatchObject({name: 'Region', value: 'eu-north', inline: true});
		expect(embed?.footer?.text).toBe('CI pipeline');
		expect(embed?.image?.url).toBeTruthy();
		expect(embed?.thumbnail?.url).toBeTruthy();
		expect(adminMessage!.webhook_id).toBe(webhook.id);
		expect(adminMessage!.author_bot).toBe(true);
	});
});
