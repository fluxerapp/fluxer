// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {UserSettingsResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createTestBotAccount} from '../../bot/tests/BotTestUtils';
import {getPngDataUrl} from '../../emoji/tests/EmojiTestUtils';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {grantPremium} from './UserTestUtils';

interface EmojiResponse {
	id: string;
}

interface CurrentUserResponse {
	premium_type?: number | null;
}

interface RpcValidateCustomStatusResponse {
	type: 'validate_custom_status';
	data: {
		custom_status: {
			text: string | null;
			expires_at: string | null;
			emoji_id: string | null;
			emoji_name: string | null;
			emoji_animated: boolean;
		} | null;
	};
}

describe('User custom status emoji premium', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('silently discards custom emoji status for non-premium users while preserving text', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Custom Status Premium Guild');
		const emojiName = 'status_emoji';
		const emoji = await createBuilder<EmojiResponse>(harness, account.token)
			.post(`/guilds/${guild.id}/emojis`)
			.body({
				name: emojiName,
				image: getPngDataUrl(),
			})
			.execute();
		const settings = await createBuilder<UserSettingsResponse>(harness, account.token)
			.patch('/users/@me/settings')
			.body({
				custom_status: {
					text: 'Status',
					emoji_id: emoji.id,
					emoji_name: `:${emojiName}:`,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(settings.custom_status?.text).toBe('Status');
		expect(settings.custom_status?.emoji_id).toBeUndefined();
		expect(settings.custom_status?.emoji_name).toBeNull();
		expect(settings.custom_status?.emoji_animated).toBe(false);
	});
	it('accepts single-codepoint unicode emoji names for custom status', async () => {
		const account = await createTestAccount(harness);
		const settings = await createBuilder<UserSettingsResponse>(harness, account.token)
			.patch('/users/@me/settings')
			.body({
				custom_status: {
					text: 'Coffee break',
					emoji_name: '☕',
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(settings.custom_status?.emoji_name).toBe('☕');
		expect(settings.custom_status?.emoji_id).toBeUndefined();
		expect(settings.custom_status?.emoji_animated).toBe(false);
	});
	it('allows custom emoji status for premium users without guild access', async () => {
		const owner = await createTestAccount(harness);
		const premiumAccount = await createTestAccount(harness);
		await grantPremium(harness, premiumAccount.userId, UserPremiumTypes.SUBSCRIPTION);
		const guild = await createGuild(harness, owner.token, 'Custom Status Premium Success Guild');
		const emojiName = 'status_emoji_premium';
		const emoji = await createBuilder<EmojiResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/emojis`)
			.body({
				name: emojiName,
				image: getPngDataUrl(),
			})
			.execute();
		const settings = await createBuilder<UserSettingsResponse>(harness, premiumAccount.token)
			.patch('/users/@me/settings')
			.body({
				custom_status: {
					text: 'Premium status',
					emoji_id: emoji.id,
					emoji_name: 'this_user_supplied_name_must_be_ignored_by_the_server',
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(settings.custom_status?.emoji_id).toBe(emoji.id);
		expect(settings.custom_status?.emoji_name).toBe(emojiName);
		expect(settings.custom_status?.text).toBe('Premium status');
	});
	it('allows custom emoji status for bots without premium or guild access', async () => {
		const botAccount = await createTestBotAccount(harness);
		const guild = await createGuild(harness, botAccount.ownerToken, 'Bot Custom Status Premium Guild');
		const emojiName = 'status_emoji_bot';
		const emoji = await createBuilder<EmojiResponse>(harness, botAccount.ownerToken)
			.post(`/guilds/${guild.id}/emojis`)
			.body({
				name: emojiName,
				image: getPngDataUrl(),
			})
			.execute();
		const botToken = `Bot ${botAccount.botToken}`;
		const botMe = await createBuilder<CurrentUserResponse>(harness, botToken).get('/users/@me').execute();
		expect(botMe.premium_type ?? 0).toBe(0);
		const validated = await createBuilder<RpcValidateCustomStatusResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'validate_custom_status',
				user_id: botAccount.botUserId,
				custom_status: {
					text: 'Bot status',
					emoji_id: emoji.id,
					emoji_name: 'rpc_user_supplied_name_must_be_ignored_by_the_server',
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(validated.type).toBe('validate_custom_status');
		expect(validated.data.custom_status?.emoji_id).toBe(emoji.id);
		expect(validated.data.custom_status?.emoji_name).toBe(emojiName);
		expect(validated.data.custom_status?.text).toBe('Bot status');
	});
});
