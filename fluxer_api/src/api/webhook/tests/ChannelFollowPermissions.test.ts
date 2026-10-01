// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {
	type AnnouncementWorld,
	addGuildMember,
	announcementWorld,
	createGuildChannel,
	follow,
	followRequest,
	grantGuildRole,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {
	createDmChannel,
	createFriendship,
	createGuild,
	createPermissionOverwrite,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {getChannelWebhooks} from '@app/api/webhook/tests/WebhookTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Channel follow permissions and content rules', () => {
	let harness: ApiTestHarness;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		world = await announcementWorld(harness);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createAnnouncement(owner: TestAccount, guildId: string, body: Record<string, unknown> = {}) {
		return createGuildChannel(harness, owner.token, guildId, {
			name: 'news-extra',
			type: ChannelTypes.GUILD_ANNOUNCEMENT,
			...body,
		});
	}

	test('requires the source to be an announcement channel', async () => {
		const {a, b} = world;
		await followRequest(harness, b.owner.token, a.guild.system_channel_id!, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
	});

	test('only accepts text channels as targets', async () => {
		const {a, b} = world;
		const category = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'category',
			type: ChannelTypes.GUILD_CATEGORY,
		});
		const link = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'link',
			type: ChannelTypes.GUILD_LINK,
			url: 'https://example.com',
		});
		const announcement = await createAnnouncement(b.owner, b.guild.id);
		for (const target of [b.voice, category, link, announcement]) {
			await followRequest(harness, b.owner.token, a.ann.id, target.id)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FOLLOW_TARGET_CHANNEL)
				.execute();
		}
	});

	test('rejects DM and group DM targets', async () => {
		const {a, b} = world;
		const friend = await createTestAccount(harness);
		await createFriendship(harness, b.owner, friend);
		const dm = await createDmChannel(harness, b.owner.token, friend.userId);
		await followRequest(harness, b.owner.token, a.ann.id, dm.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FOLLOW_TARGET_CHANNEL)
			.execute();
		const other = await createTestAccount(harness);
		await createFriendship(harness, b.owner, other);
		const groupDm = await createBuilder<ChannelResponse>(harness, b.owner.token)
			.post('/users/@me/channels')
			.body({recipients: [friend.userId, other.userId]})
			.execute();
		await followRequest(harness, b.owner.token, a.ann.id, groupDm.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FOLLOW_TARGET_CHANNEL)
			.execute();
	});

	test('requires membership and View Channel on the source', async () => {
		const {a, b} = world;
		const outsider = await createTestAccount(harness);
		const outsiderGuild = await createGuild(harness, outsider.token, 'Outsider Guild');
		await followRequest(harness, outsider.token, a.ann.id, outsiderGuild.system_channel_id!)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
			.execute();
		await createPermissionOverwrite(harness, a.owner.token, a.ann.id, b.owner.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	test('requires Manage Webhooks in the target guild and channel', async () => {
		const {a, b} = world;
		const plain = await createTestAccount(harness);
		await addGuildMember(harness, a.owner, a.guild, plain);
		await addGuildMember(harness, b.owner, b.guild, plain);
		await followRequest(harness, plain.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await grantGuildRole(harness, b.owner, b.guild.id, plain, Permissions.MANAGE_WEBHOOKS, 'Hooks');
		await createPermissionOverwrite(harness, b.owner.token, b.t2.id, plain.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.MANAGE_WEBHOOKS.toString(),
		});
		await followRequest(harness, plain.token, a.ann.id, b.t2.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await follow(harness, plain.token, a.ann.id, b.t1.id);
	});

	test('a channel-only Manage Webhooks grant still needs the guild permission', async () => {
		const {a, b} = world;
		await followRequest(harness, b.webhookManager.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await grantGuildRole(harness, b.owner, b.guild.id, b.webhookManager, Permissions.MANAGE_WEBHOOKS, 'Hooks');
		await createPermissionOverwrite(harness, b.owner.token, b.t2.id, b.webhookManager.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.MANAGE_WEBHOOKS.toString(),
		});
		await follow(harness, b.webhookManager.token, a.ann.id, b.t1.id);
		await followRequest(harness, b.webhookManager.token, a.ann.id, b.t2.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	test('an age-restricted source only posts into age-restricted targets', async () => {
		const {a, b} = world;
		const restrictedSource = await createAnnouncement(a.owner, a.guild.id, {nsfw_override: true});
		await followRequest(harness, b.owner.token, restrictedSource.id, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED)
			.execute();
		await followRequest(harness, b.owner.token, restrictedSource.id, b.contentWarning.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED)
			.execute();
		await follow(harness, b.owner.token, restrictedSource.id, b.ageRestricted.id);
	});

	test('age restriction inherited from a category or guild counts on both sides', async () => {
		const {a, b} = world;
		const sourceCategory = await createGuildChannel(harness, a.owner.token, a.guild.id, {
			name: 'adult',
			type: ChannelTypes.GUILD_CATEGORY,
			nsfw_override: true,
		});
		const inheritedSource = await createAnnouncement(a.owner, a.guild.id, {parent_id: sourceCategory.id});
		await followRequest(harness, b.owner.token, inheritedSource.id, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED)
			.execute();
		const targetCategory = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'adult-target',
			type: ChannelTypes.GUILD_CATEGORY,
			nsfw_override: true,
		});
		const inheritedTarget = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'inherits',
			type: ChannelTypes.GUILD_TEXT,
			parent_id: targetCategory.id,
		});
		await follow(harness, b.owner.token, inheritedSource.id, inheritedTarget.id);

		const adultOwner = await createTestAccount(harness);
		const adultGuild = await createGuild(harness, adultOwner.token, 'Adult Guild');
		await updateGuild(harness, adultOwner.token, adultGuild.id, {nsfw: true});
		const guildSource = await createAnnouncement(adultOwner, adultGuild.id);
		await addGuildMember(harness, adultOwner, adultGuild, b.owner);
		await followRequest(harness, b.owner.token, guildSource.id, b.t2.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED)
			.execute();
		const adultTarget = await createGuildChannel(harness, adultOwner.token, adultGuild.id, {
			name: 'adult-text',
			type: ChannelTypes.GUILD_TEXT,
		});
		await follow(harness, adultOwner.token, guildSource.id, adultTarget.id);
		await addGuildMember(harness, a.owner, a.guild, adultOwner);
		await follow(harness, adultOwner.token, inheritedSource.id, adultTarget.id);
	});

	test('a content-warning source only posts into warned or age-restricted targets', async () => {
		const {a, b} = world;
		const warnedSource = await createAnnouncement(a.owner, a.guild.id, {
			content_warning_level: ContentWarningLevel.CONTENT_WARNING,
		});
		await followRequest(harness, b.owner.token, warnedSource.id, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FOLLOW_TARGET_CONTENT_WARNING_REQUIRED)
			.execute();
		await follow(harness, b.owner.token, warnedSource.id, b.contentWarning.id);
		await follow(harness, b.owner.token, warnedSource.id, b.ageRestricted.id);
		const warnedGuildOwner = await createTestAccount(harness);
		const warnedGuild = await createGuild(harness, warnedGuildOwner.token, 'Warned Guild');
		await updateGuild(harness, warnedGuildOwner.token, warnedGuild.id, {
			content_warning_level: ContentWarningLevel.CONTENT_WARNING,
		});
		await addGuildMember(harness, a.owner, a.guild, warnedGuildOwner);
		await follow(harness, warnedGuildOwner.token, warnedSource.id, warnedGuild.system_channel_id!);
	});

	test('an unrestricted source may post into an age-restricted target', async () => {
		const {a, b} = world;
		await follow(harness, b.owner.token, a.ann.id, b.ageRestricted.id);
	});

	test('an age-restricted source requires an age-verified caller', async () => {
		const {a, b} = world;
		const restrictedSource = await createAnnouncement(a.owner, a.guild.id, {nsfw_override: true});
		const minor = await createTestAccount(harness, {dateOfBirth: '2011-06-15'});
		await addGuildMember(harness, a.owner, a.guild, minor);
		await addGuildMember(harness, b.owner, b.guild, minor);
		await grantGuildRole(harness, b.owner, b.guild.id, minor, Permissions.MANAGE_WEBHOOKS, 'Hooks');
		await followRequest(harness, minor.token, restrictedSource.id, b.ageRestricted.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED)
			.execute();
	});

	test('a bot with Manage Webhooks can follow', async () => {
		const {b} = world;
		const bot = await createTestBotAccount(harness);
		const sourceGuild = await createGuild(harness, bot.ownerToken, 'Bot Source');
		const source = await createGuildChannel(harness, bot.ownerToken, sourceGuild.id, {
			name: 'bot-news',
			type: ChannelTypes.GUILD_ANNOUNCEMENT,
		});
		await authorizeBot(
			harness,
			bot.ownerToken,
			bot.appId,
			['bot'],
			sourceGuild.id,
			Permissions.VIEW_CHANNEL.toString(),
		);
		await authorizeBot(
			harness,
			b.owner.token,
			bot.appId,
			['bot'],
			b.guild.id,
			(Permissions.VIEW_CHANNEL | Permissions.MANAGE_WEBHOOKS).toString(),
		);
		const followed = await follow(harness, `Bot ${bot.botToken}`, source.id, b.t1.id);
		const webhooks = await getChannelWebhooks(harness, b.t1.id, b.owner.token);
		const webhook = webhooks.find((candidate) => candidate.id === followed.webhook_id);
		expect(webhook?.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
		expect(webhook?.user.id).toBe(bot.botUserId);
	});
});
