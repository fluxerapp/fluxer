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

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@fluxer/api/src/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {
	createGuildEmoji,
	createWebhook,
	deleteWebhook,
	executeWebhook,
	getChannelMessage,
	grantCreateExpressionsPermission,
	grantStaffAccess,
	sendChannelMessage,
} from '@fluxer/api/src/webhook/tests/WebhookTestUtils';
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
