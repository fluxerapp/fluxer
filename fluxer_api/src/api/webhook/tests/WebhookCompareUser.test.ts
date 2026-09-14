// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createGuildEmoji,
	createWebhook,
	deleteWebhook,
	executeWebhook,
	getChannelMessage,
	grantCreateExpressionsPermission,
	grantStaffAccess,
	sendChannelMessage,
} from '@app/api/webhook/tests/WebhookTestUtils';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('Webhook compare to regular user', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	it('webhook can use external emoji while regular user cannot', async () => {
		const user = await createTestAccount(harness);
		await ensureSessionStarted(harness, user.token);
		const guild = await createGuild(harness, user.token, 'Comparison Test Guild');
		const channelId = guild.system_channel_id ?? (await createChannel(harness, user.token, guild.id, 'general')).id;
		const emojiGuild = await createGuild(harness, user.token, 'Emoji Source Guild');
		const emojiGuildId = emojiGuild.id;
		await grantStaffAccess(harness, user.userId);
		await ensureSessionStarted(harness, user.token);
		await grantCreateExpressionsPermission(harness, user.token, emojiGuildId);
		const emoji = await createGuildEmoji(harness, user.token, emojiGuildId, 'compare');
		const webhook = await createWebhook(harness, channelId, user.token, 'Comparison Webhook');
		const emojiContent = `Test <:compare:${emoji.id}>`;
		const webhookResult = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{
				content: emojiContent,
				wait: true,
			},
			200,
		);
		expect(webhookResult.response.status).toBe(200);
		expect(webhookResult.json).not.toBeNull();
		expect(webhookResult.json!.content).toContain('<:compare:');
		const msg = await sendChannelMessage(harness, user.token, channelId, emojiContent);
		const fetched = await getChannelMessage(harness, user.token, channelId, msg.id);
		expect(fetched.content).not.toContain('<:compare:');
		expect(fetched.content).toContain(':compare:');
		await deleteWebhook(harness, webhook.id, user.token);
	});
});
