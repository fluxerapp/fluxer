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
import {createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {
	createGuildEmoji,
	createGuildEmojiWithFile,
	createWebhook,
	deleteWebhook,
	executeWebhook,
	grantCreateExpressionsPermission,
	grantStaffAccess,
} from '@fluxer/api/src/webhook/tests/WebhookTestUtils';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Webhook emoji bypass', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	it('webhook can use external emoji without sanitization', async () => {
		const user = await createTestAccount(harness);
		const guild = await createGuild(harness, user.token, 'Webhook Emoji Test Guild');
		const guildId = guild.id;
		const channelId = guild.system_channel_id!;

		await grantStaffAccess(harness, user.userId);
		await grantCreateExpressionsPermission(harness, user.token, guildId);

		const emoji = await createGuildEmoji(harness, user.token, guildId, 'external');
		const animatedEmoji = await createGuildEmojiWithFile(
			harness,
			user.token,
			guildId,
			'animated',
			'thisisfine.gif',
			'image/gif',
		);

		const webhook = await createWebhook(harness, channelId, user.token, 'Emoji Test Webhook');

		const messagePayload = `<:external:${emoji.id}> <a:animated:${animatedEmoji.id}>`;

		const result = await executeWebhook(harness, webhook.id, webhook.token, {
			content: `Webhook message ${messagePayload}`,
		});

		expect(result.response.status).toBe(204);

		await deleteWebhook(harness, webhook.id, user.token);
	});

	it('webhook can use non-existent emoji', async () => {
		const user = await createTestAccount(harness);
		const guild = await createGuild(harness, user.token, 'Webhook Emoji Test Guild');
		const channelId = guild.system_channel_id!;

		const webhook = await createWebhook(harness, channelId, user.token, 'Emoji Test Webhook');

		const result = await executeWebhook(harness, webhook.id, webhook.token, {
			content: 'Fake emoji <:doesnotexist:123456789012345678>',
		});

		expect(result.response.status).toBe(204);

		await deleteWebhook(harness, webhook.id, user.token);
	});

	it('webhook emoji in code block', async () => {
		const user = await createTestAccount(harness);
		const guild = await createGuild(harness, user.token, 'Webhook Emoji Test Guild');
		const channelId = guild.system_channel_id!;

		const webhook = await createWebhook(harness, channelId, user.token, 'Emoji Test Webhook');

		const result = await executeWebhook(harness, webhook.id, webhook.token, {
			content: 'Code: `<:code_emoji:111111111111111111>`',
		});

		expect(result.response.status).toBe(204);

		await deleteWebhook(harness, webhook.id, user.token);
	});

	it('webhook wait parameter returns message', async () => {
		const user = await createTestAccount(harness);
		const guild = await createGuild(harness, user.token, 'Webhook Emoji Test Guild');
		const guildId = guild.id;
		const channelId = guild.system_channel_id!;

		await grantStaffAccess(harness, user.userId);
		await grantCreateExpressionsPermission(harness, user.token, guildId);

		const emoji = await createGuildEmoji(harness, user.token, guildId, 'wait_emoji');

		const webhook = await createWebhook(harness, channelId, user.token, 'Emoji Test Webhook');

		const result = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{
				content: `Wait test <:wait_emoji:${emoji.id}>`,
				wait: true,
			},
			200,
		);

		expect(result.response.status).toBe(200);
		expect(result.json).not.toBeNull();
		expect(result.json!.id).toBeDefined();

		await deleteWebhook(harness, webhook.id, user.token);
	});
});
