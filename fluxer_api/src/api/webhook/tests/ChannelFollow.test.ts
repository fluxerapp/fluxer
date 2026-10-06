// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID, createWebhookID, createWebhookToken} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	type AnnouncementWorld,
	addGuildMember,
	announcementWorld,
	createAnnouncementSourceGuild,
	createGuildChannel,
	follow,
	followRequest,
	setGuildFeatures,
	setLimitOverride,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {createGuild, createPermissionOverwrite, updateGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, getChannelWebhooks} from '@app/api/webhook/tests/WebhookTestUtils';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, MessageTypes, Permissions, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_WEBHOOKS_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {ChannelFollowerStatsResponse} from '@fluxer/schema/src/domains/channel/ChannelFollowSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {WebhookListResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

async function listMessages(harness: ApiTestHarness, token: string, channelId: string) {
	return createBuilder<Array<MessageResponse>>(harness, token).get(`/channels/${channelId}/messages`).execute();
}

async function listAuditLogs(harness: ApiTestHarness, token: string, guildId: string, actionType: number) {
	return createBuilder<GuildAuditLogListResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs?action_type=${actionType}`)
		.execute();
}

async function followerStats(harness: ApiTestHarness, token: string, channelId: string) {
	return createBuilder<ChannelFollowerStatsResponse>(harness, token)
		.get(`/channels/${channelId}/follower-stats`)
		.execute();
}

describe('Channel follow', () => {
	let harness: ApiTestHarness;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		world = await announcementWorld(harness);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('creates a follower webhook with the source name and a copy of the source icon', async () => {
		const {a, b} = world;
		const withIcon = await updateGuild(harness, a.owner.token, a.guild.id, {icon: getPngDataUrl()});
		expect(withIcon.icon).toBeTruthy();
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		expect(followed.channel_id).toBe(a.ann.id);
		const webhooks = WebhookListResponse.parse(await getChannelWebhooks(harness, b.t1.id, b.owner.token));
		const webhook = webhooks.find((candidate) => candidate.id === followed.webhook_id);
		expect(webhook).toBeDefined();
		expect(webhook?.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
		expect(webhook).not.toHaveProperty('token');
		expect(webhook?.name).toBe(`${withIcon.name} #news`);
		expect(webhook?.avatar).toBe(withIcon.icon);
		expect(webhook?.source_guild).toEqual({id: a.guild.id, name: withIcon.name, icon: withIcon.icon});
		expect(webhook?.source_channel).toEqual({id: a.ann.id, name: 'news'});
		const iconKey = withIcon.icon!.replace(/^a_/, '');
		expect(harness.storageService.getCopiedObjects()).toContainEqual({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey: `icons/${a.guild.id}/${iconKey}`,
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey: `avatars/${followed.webhook_id}/${iconKey}`,
		});
		const stored = await new WebhookRepository().findUnique(createWebhookID(BigInt(followed.webhook_id)));
		expect(stored?.sourceGuildId?.toString()).toBe(a.guild.id);
		expect(stored?.sourceChannelId?.toString()).toBe(a.ann.id);
		expect(stored?.creatorId?.toString()).toBe(b.owner.userId);
	});

	test('uses a null avatar when the source guild has no icon', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const webhooks = await getChannelWebhooks(harness, b.t1.id, b.owner.token);
		expect(webhooks.find((candidate) => candidate.id === followed.webhook_id)?.avatar ?? null).toBeNull();
		expect(
			harness.storageService
				.getCopiedObjects()
				.filter((copy) => copy.destinationKey.startsWith(`avatars/${followed.webhook_id}/`)),
		).toHaveLength(0);
	});

	test('rejects a limited account and creates no webhook', async () => {
		const {a, b} = world;
		await createBuilder(harness, '')
			.post(`/test/users/${b.owner.userId}/security-flags`)
			.body({set_flags: ['ACCOUNT_LIMITED']})
			.execute();
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_LIMITED)
			.execute();
		await createBuilder(harness, '')
			.post(`/test/users/${b.owner.userId}/security-flags`)
			.body({clear_flags: ['ACCOUNT_LIMITED']})
			.execute();
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(0);
	});

	test('truncates long follower webhook names to 80 code points', async () => {
		const {b} = world;
		const longName = `${'\u{1F4E3}'.repeat(10)}${'x'.repeat(80)}`;
		const source = await createAnnouncementSourceGuild(harness, longName.slice(0, 90));
		await addGuildMember(harness, source.owner, source.guild, b.owner);
		const followed = await follow(harness, b.owner.token, source.ann.id, b.t1.id);
		const webhooks = await getChannelWebhooks(harness, b.t1.id, b.owner.token);
		const name = webhooks.find((candidate) => candidate.id === followed.webhook_id)!.name;
		expect(name.length).toBeLessThanOrEqual(80);
		expect(name.length).toBeGreaterThanOrEqual(79);
		expect(name.startsWith('\u{1F4E3}'.repeat(10))).toBe(true);
		expect(Array.from(name).every((codePoint) => codePoint.length === 2 || codePoint.charCodeAt(0) < 0xd800)).toBe(
			true,
		);
	});

	test('falls back to the channel name when the combined name fails moderation', async () => {
		const {b} = world;
		const source = await createAnnouncementSourceGuild(harness, 'Forbidden Words Guild');
		await addGuildMember(harness, source.owner, source.guild, b.owner);
		vi.spyOn(phraseBlocklistCache, 'containsBannedPhrase').mockImplementation((text) =>
			text.includes('Forbidden Words'),
		);
		const followed = await follow(harness, b.owner.token, source.ann.id, b.t1.id);
		vi.restoreAllMocks();
		const webhooks = await getChannelWebhooks(harness, b.t1.id, b.owner.token);
		expect(webhooks.find((candidate) => candidate.id === followed.webhook_id)?.name).toBe('#news');
	});

	test('rejects the follow when the fallback name also fails moderation', async () => {
		const {a, b} = world;
		vi.spyOn(phraseBlocklistCache, 'containsBannedPhrase').mockImplementation((text) => text.includes('news'));
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
		vi.restoreAllMocks();
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(0);
	});

	test('posts a CHANNEL_FOLLOW_ADD system message whose reference has no message id', async () => {
		const {a, b} = world;
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const webhookUpdates = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'WEBHOOKS_UPDATE');
		expect(webhookUpdates).toEqual([
			expect.objectContaining({guildId: createGuildID(BigInt(b.guild.id)), data: {channel_id: b.t1.id}}),
		]);
		const webhooks = await getChannelWebhooks(harness, b.t1.id, b.owner.token);
		const webhookName = webhooks.find((candidate) => candidate.id === followed.webhook_id)!.name;
		const messages = await listMessages(harness, b.owner.token, b.t1.id);
		const systemMessage = messages.find((message) => message.type === MessageTypes.CHANNEL_FOLLOW_ADD);
		expect(systemMessage).toBeDefined();
		expect(systemMessage?.author.id).toBe(b.owner.userId);
		expect(systemMessage?.content).toBe(webhookName);
		expect(systemMessage?.mention_everyone).toBe(false);
		expect(systemMessage?.message_reference).toEqual({type: 0, guild_id: a.guild.id, channel_id: a.ann.id});
		expect(systemMessage?.message_reference).not.toHaveProperty('message_id');
		expect(systemMessage).not.toHaveProperty('referenced_message');
		await ensureSessionStarted(harness, b.owner.token);
		await createBuilder(harness, b.owner.token)
			.patch(`/channels/${b.t1.id}/messages/${systemMessage!.id}`)
			.body({content: 'edited'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CANNOT_MODIFY_SYSTEM_WEBHOOK)
			.execute();
		await createBuilder(harness, b.owner.token)
			.delete(`/channels/${b.t1.id}/messages/${systemMessage!.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const remaining = await listMessages(harness, b.owner.token, b.t1.id);
		expect(remaining.some((message) => message.id === systemMessage!.id)).toBe(false);
	});

	test('records WEBHOOK_CREATE in the target guild only, with the reason and follower type', async () => {
		const {a, b} = world;
		const followed = await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.header('X-Audit-Log-Reason', 'release notes')
			.execute();
		const targetLog = await listAuditLogs(harness, b.owner.token, b.guild.id, AuditLogActionType.WEBHOOK_CREATE);
		const entry = targetLog.audit_log_entries.find((candidate) => candidate.target_id === followed.webhook_id);
		expect(entry?.reason).toBe('release notes');
		expect(entry?.user_id).toBe(b.owner.userId);
		expect(entry?.options).toEqual({channel_id: b.t1.id, type: WebhookTypes.CHANNEL_FOLLOWER});
		const sourceLog = await listAuditLogs(harness, a.owner.token, a.guild.id, AuditLogActionType.WEBHOOK_CREATE);
		expect(sourceLog.audit_log_entries).toHaveLength(0);
	});

	test('allows following into a text channel of the same guild', async () => {
		const {a} = world;
		const local = await createGuildChannel(harness, a.owner.token, a.guild.id, {
			name: 'local',
			type: ChannelTypes.GUILD_TEXT,
		});
		const followed = await follow(harness, a.owner.token, a.ann.id, local.id);
		expect(followed.channel_id).toBe(a.ann.id);
	});

	test('rejects a duplicate follow and allows another target', async () => {
		const {a, b} = world;
		await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CHANNEL_ALREADY_FOLLOWED)
			.execute();
		await follow(harness, b.owner.token, a.ann.id, b.t2.id);
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(1);
	});

	test('allows two sources to post into the same target', async () => {
		const {a, b} = world;
		const second = await createGuildChannel(harness, a.owner.token, a.guild.id, {
			name: 'updates',
			type: ChannelTypes.GUILD_ANNOUNCEMENT,
		});
		await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		await follow(harness, b.owner.token, second.id, b.t1.id);
		const webhooks = await getChannelWebhooks(harness, b.t1.id, b.owner.token);
		expect(webhooks.map((webhook) => webhook.source_channel?.id).sort()).toEqual([a.ann.id, second.id].sort());
	});

	test('counts follower webhooks toward the channel webhook cap', async () => {
		const {a, b} = world;
		for (let index = 0; index < MAX_WEBHOOKS_PER_CHANNEL; index++) {
			await createWebhook(harness, b.t1.id, b.owner.token, `Hook ${index}`);
		}
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.MAX_WEBHOOKS_PER_CHANNEL)
			.execute();
	});

	test('counts follower webhooks toward the guild webhook cap', async () => {
		const {a, b} = world;
		const restore = await setLimitOverride(harness, {max_webhooks_per_guild: 1});
		try {
			await createWebhook(harness, b.t2.id, b.owner.token, 'Existing');
			await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.MAX_WEBHOOKS_PER_GUILD)
				.execute();
		} finally {
			await restore();
		}
	});

	test('an announcement channel takes follows however many it already has', async () => {
		const {a, b} = world;
		vi.spyOn(WebhookRepository.prototype, 'countBySourceChannel').mockResolvedValue({
			channelCount: 100_000,
			guildCount: 50_000,
		});
		try {
			await follow(harness, b.owner.token, a.ann.id, b.t1.id);
			await follow(harness, b.owner.token, a.ann.id, b.t2.id);
		} finally {
			vi.restoreAllMocks();
		}
		expect(await new WebhookRepository().countBySourceChannel(createChannelID(BigInt(a.ann.id)))).toEqual({
			channelCount: 2,
			guildCount: 1,
		});
	});

	test('one user can create more than 20 follows in a row', async () => {
		const {a, b} = world;
		const sources = [a.ann];
		for (let index = 1; index < 7; index++) {
			sources.push(
				await createGuildChannel(harness, a.owner.token, a.guild.id, {
					name: `news-${index}`,
					type: ChannelTypes.GUILD_ANNOUNCEMENT,
				}),
			);
		}
		const third = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'third',
			type: ChannelTypes.GUILD_TEXT,
		});
		const pairs: Array<[ChannelResponse, ChannelResponse]> = [];
		for (const target of [b.t1, b.t2, third]) {
			for (const source of sources) {
				pairs.push([source, target]);
			}
		}
		expect(pairs.length).toBeGreaterThan(20);
		for (const [source, target] of pairs) {
			await follow(harness, b.owner.token, source.id, target.id);
		}
		for (const target of [b.t1, b.t2, third]) {
			const webhooks = await getChannelWebhooks(harness, target.id, b.owner.token);
			expect(webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER)).toHaveLength(sources.length);
		}
	});
	test('reports follower stats, cached for a minute and refreshed by a follow', async () => {
		const {a, b} = world;
		await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		await follow(harness, b.owner.token, a.ann.id, b.t2.id);
		expect(await followerStats(harness, a.member.token, a.ann.id)).toEqual({channel_count: 2, guild_count: 1});
		await new WebhookRepository().create({
			webhookId: createWebhookID(BigInt(b.voice.id) + 7n),
			token: createWebhookToken('s'.repeat(64)),
			type: WebhookTypes.CHANNEL_FOLLOWER,
			guildId: createGuildID(BigInt(b.guild.id)),
			channelId: createChannelID(BigInt(b.ageRestricted.id)),
			creatorId: createUserID(BigInt(b.owner.userId)),
			name: 'Direct',
			avatarHash: null,
			sourceGuildId: createGuildID(BigInt(a.guild.id)),
			sourceChannelId: createChannelID(BigInt(a.ann.id)),
		});
		expect(await followerStats(harness, a.member.token, a.ann.id)).toEqual({channel_count: 2, guild_count: 1});
		const c = await createTestAccount(harness);
		const guildC = await createGuild(harness, c.token, 'Guild C');
		await addGuildMember(harness, a.owner, a.guild, c);
		await follow(harness, c.token, a.ann.id, guildC.system_channel_id!);
		expect(await followerStats(harness, a.member.token, a.ann.id)).toEqual({channel_count: 4, guild_count: 2});
	});

	test('rejects follower stats outside announcement channels and without access', async () => {
		const {a, b} = world;
		await createBuilder(harness, a.owner.token)
			.get(`/channels/${a.guild.system_channel_id}/follower-stats`)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
		const outsider = await createTestAccount(harness);
		await createBuilder(harness, outsider.token)
			.get(`/channels/${a.ann.id}/follower-stats`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
			.execute();
		await createPermissionOverwrite(harness, a.owner.token, a.ann.id, b.owner.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await createBuilder(harness, b.owner.token)
			.get(`/channels/${a.ann.id}/follower-stats`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	test('lets exactly one of two concurrent identical follows succeed', async () => {
		const {a, b} = world;
		const attempts = await Promise.all([
			followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
				.expect(HTTP_STATUS.OK)
				.executeWithResponse()
				.then(
					() => 'ok',
					(error: Error) => error.message,
				),
			followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
				.expect(HTTP_STATUS.OK)
				.executeWithResponse()
				.then(
					() => 'ok',
					(error: Error) => error.message,
				),
		]);
		expect(attempts.filter((result) => result === 'ok')).toHaveLength(1);
		const failure = attempts.find((result) => result !== 'ok')!;
		expect(failure).toMatch(/CHANNEL_ALREADY_FOLLOWED|RESOURCE_LOCKED/);
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(1);
	});

	test('never leaves a follower webhook inside a channel converted to announcement concurrently', async () => {
		const {a, b} = world;
		await Promise.allSettled([
			follow(harness, b.owner.token, a.ann.id, b.t1.id),
			createBuilder(harness, b.owner.token).patch(`/channels/${b.t1.id}`).body({type: 5}).execute(),
		]);
		const channel = await createBuilder<ChannelResponse>(harness, b.owner.token).get(`/channels/${b.t1.id}`).execute();
		const webhooks = await new WebhookRepository().listByChannel(createChannelID(BigInt(b.t1.id)));
		const followers = webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER);
		if (channel.type === ChannelTypes.GUILD_ANNOUNCEMENT) {
			expect(followers).toHaveLength(0);
		} else {
			expect(followers).toHaveLength(1);
		}
	});

	test('undoes a follow whose source stopped being an announcement channel mid-request', async () => {
		const {a, b} = world;
		const originalCreate = WebhookRepository.prototype.create;
		vi.spyOn(WebhookRepository.prototype, 'create').mockImplementation(async function (this: WebhookRepository, data) {
			const created = await originalCreate.call(this, data);
			await createBuilder(harness, a.owner.token).patch(`/channels/${a.ann.id}`).body({type: 0}).execute();
			return created;
		});
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
		vi.restoreAllMocks();
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(0);
		expect(await new WebhookRepository().countBySourceChannel(createChannelID(BigInt(a.ann.id)))).toEqual({
			channelCount: 0,
			guildCount: 0,
		});
		const messages = await listMessages(harness, b.owner.token, b.t1.id);
		expect(messages.some((message) => message.type === MessageTypes.CHANNEL_FOLLOW_ADD)).toBe(false);
	});

	test('refuses to follow while the source guild has announcement channels disabled', async () => {
		const {a, b} = world;
		await setGuildFeatures(harness, a.guild.id, {add: [GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED]});
		await followRequest(harness, b.owner.token, a.ann.id, b.t1.id)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.FEATURE_TEMPORARILY_DISABLED)
			.execute();
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(0);
	});
});
