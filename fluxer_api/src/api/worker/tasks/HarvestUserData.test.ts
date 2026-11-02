// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createEmojiID,
	createGuildID,
	createMessageID,
	createUserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {ThreadMemberRow} from '@app/api/database/types/ThreadTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {clearChannelThreadsTaintCacheForTesting, insertGuildThreadMarker} from '@app/api/experiment/ChannelThreadsGate';
import {getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getStoreBillingRepository} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCertificateService} from '@app/api/operator_pass/OperatorPassCertificateService';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {buildStorePurchaseRow} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {
	GuildMembersByUserId,
	Messages,
	MessagesByAuthorV2,
	ThreadMembers,
	ThreadMembersByUser,
	UserRecoveryKits,
} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HarvestedMessagesSchema, harvestUserArchive} from '@app/api/test/UserArchiveTestUtils';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies, type WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {makeBucket} from '@fluxer/snowflake/src/SnowflakeBuckets';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {z} from 'zod';

const AUTHOR = createUserID(1000000000000000000n);
let harness: ApiTestHarness;
let deps: WorkerDependencies;
let selfHosted: boolean;

beforeAll(async () => {
	harness = await createApiTestHarness();
});
beforeEach(async () => {
	await harness.reset();
	clearChannelThreadsTaintCacheForTesting();
	selfHosted = Config.instance.selfHosted;
	deps = await initializeWorkerDependencies(getSnowflakeService());
	setWorkerDependencies(deps);
	await deps.userRepository.create({
		...EMPTY_USER_ROW,
		user_id: AUTHOR,
		username: 'exporter',
		discriminator: 1,
		email: 'operator@example.com',
	});
});
afterEach(() => {
	Config.instance.selfHosted = selfHosted;
	clearChannelThreadsTaintCacheForTesting();
	clearWorkerDependencies();
});
afterAll(async () => {
	await harness.shutdown();
});

function messageRow(channelId: ChannelID, id: bigint): MessageRow {
	const messageId = createMessageID(id);
	return {
		channel_id: channelId,
		bucket: makeBucket(messageId),
		message_id: messageId,
		author_id: AUTHOR,
		type: 0,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: `body ${id}`,
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		has_reaction: false,
		version: 1,
	};
}

async function seedMessages(channelId: ChannelID, count: number, firstId = 1500000000000000000n) {
	for (let index = 0; index < count; index++) {
		const row = messageRow(channelId, firstId + BigInt(index));
		await upsertOne(Messages.upsertAll(row));
		await upsertOne(
			MessagesByAuthorV2.upsertAll({author_id: AUTHOR, channel_id: channelId, message_id: row.message_id}),
		);
	}
}

describe('harvested messages', () => {
	it('archives every message beyond the first page', async () => {
		await seedMessages(createChannelID(2000000000000000000n), 2500);
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		const messages = HarvestedMessagesSchema.parse(archive.readJson('channels/2000000000000000000/messages.json'));
		expect(messages).toHaveLength(2500);
		expect(new Set(messages.map((message) => message.id)).size).toBe(2500);
		expect(messages).toContainEqual({id: '1500000000000000000', content: 'body 1500000000000000000'});
		expect(messages).toContainEqual({id: '1500000000000002499', content: 'body 1500000000000002499'});
	});

	it('groups messages into the archive file for their channel', async () => {
		await seedMessages(createChannelID(2000000000000000000n), 4);
		await seedMessages(createChannelID(2000000000000000001n), 4, 1500000000000000004n);
		await seedMessages(createChannelID(2000000000000000002n), 4, 1500000000000000008n);
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		for (const name of [
			'channels/2000000000000000000/messages.json',
			'channels/2000000000000000001/messages.json',
			'channels/2000000000000000002/messages.json',
		]) {
			expect(HarvestedMessagesSchema.parse(archive.readJson(name))).toHaveLength(4);
		}
	});

	it('leaves out stale author references to missing messages', async () => {
		const channelId = createChannelID(2000000000000000000n);
		await seedMessages(channelId, 2);
		await upsertOne(
			MessagesByAuthorV2.upsertAll({
				author_id: AUTHOR,
				channel_id: channelId,
				message_id: createMessageID(1500000000000000002n),
			}),
		);
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		expect(HarvestedMessagesSchema.parse(archive.readJson('channels/2000000000000000000/messages.json'))).toEqual([
			{id: '1500000000000000001', content: 'body 1500000000000000001'},
			{id: '1500000000000000000', content: 'body 1500000000000000000'},
		]);
	});

	it('creates no channel files for an account without messages', async () => {
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		expect(Object.keys(archive.entries).sort()).toEqual([
			'account/security.json',
			'integrations/oauth.json',
			'payments/payment_history.json',
			'user.json',
		]);
	});
});

function threadMemberRow(overrides: Partial<ThreadMemberRow> = {}): ThreadMemberRow {
	return {
		thread_id: createChannelID(1400000000000000010n),
		user_id: AUTHOR,
		guild_id: createGuildID(1400000000000000000n),
		parent_id: createChannelID(1400000000000000001n),
		join_timestamp: new Date('2026-09-27T00:00:00.000Z'),
		flags: 1,
		muted: false,
		mute_config: null,
		...overrides,
	};
}

describe('harvested thread memberships', () => {
	it('includes joined threads from marked guilds and omits stale rows from unmarked guilds', async () => {
		const marked = threadMemberRow();
		const unmarked = threadMemberRow({
			guild_id: createGuildID(1400000000000000002n),
			thread_id: createChannelID(1400000000000000020n),
		});
		for (const member of [marked, unmarked]) {
			await upsertOne(GuildMembersByUserId.upsertAll({user_id: AUTHOR, guild_id: member.guild_id}));
			await upsertOne(ThreadMembers.upsertAll(member));
			await upsertOne(
				ThreadMembersByUser.upsertAll({
					user_id: AUTHOR,
					guild_id: member.guild_id,
					parent_id: member.parent_id,
					is_private: false,
					thread_id: member.thread_id,
				}),
			);
		}
		await insertGuildThreadMarker(marked.guild_id, null);
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		expect(archive.readJson('user.json')).toMatchObject({
			thread_memberships: [
				{
					thread_id: '1400000000000000010',
					guild_id: '1400000000000000000',
					parent_id: '1400000000000000001',
					join_timestamp: '2026-09-27T00:00:00.000Z',
					flags: 1,
					muted: false,
				},
			],
		});
	});
});

interface SeedData {
	storePurchases?: Array<StorePurchaseRow>;
	recoveryKitCreatedAt?: Date;
}

const UserDataSchema = z.looseObject({});

async function exportUserData(data: SeedData = {}) {
	for (const purchase of data.storePurchases ?? []) await getStoreBillingRepository().insertPurchase(purchase);
	if (data.recoveryKitCreatedAt) {
		Config.instance.selfHosted = true;
		await deps.instanceConfigRepository.setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		await upsertOne(
			UserRecoveryKits.upsertAll({
				user_id: AUTHOR,
				secret_hash: 'recovery-secret',
				created_at: data.recoveryKitCreatedAt,
			}),
		);
	}
	const archive = await harvestUserArchive(harness, deps, AUTHOR);
	return UserDataSchema.parse(archive.readJson('user.json'));
}

describe('harvested user data', () => {
	it('exports the store purchases in their public shape', async () => {
		const purchase = buildStorePurchaseRow({
			user_id: AUTHOR,
			id: 1234n,
			created_at: new Date('2026-11-01T00:00:00Z'),
			expires_at: new Date('2026-12-01T00:00:00Z'),
		});
		const data = await exportUserData({storePurchases: [purchase]});
		expect(data.store_purchases).toEqual([
			{
				id: '1234',
				provider: 'app_store',
				kind: 'subscription',
				slot: 'monthly',
				product_id: 'com.fluxer.plutonium.monthly',
				environment: 'production',
				state: 'active',
				entitled: true,
				expires_at: '2026-12-01T00:00:00.000Z',
				entitled_until: '2026-12-01T00:00:00.000Z',
				will_renew: true,
				gift_code: null,
				created_at: '2026-11-01T00:00:00.000Z',
			},
		]);
	});

	it('exports when the recovery kit was created and nothing else about it', async () => {
		const createdAt = new Date('2026-10-01T12:00:00.000Z');
		const data = await exportUserData({recoveryKitCreatedAt: createdAt});
		expect(data.recovery_kit).toEqual({created_at: '2026-10-01T12:00:00.000Z'});
	});

	it('exports every kept certificate copy, and only the name and file that survived an objection', async () => {
		const repository = new OperatorPassRepository();
		const purchase = {
			checkout_session_id: 'cs_test_harvest_certificates',
			purchaser_id: AUTHOR,
			quantity: 1,
			status: 'completed',
			phase: 'standard',
			amount_minor: 2900,
			currency: 'EUR',
			quote_honored: false,
			created_at: new Date('2026-11-01T10:00:00Z'),
			purchased_at: new Date('2026-11-01T10:00:00Z'),
			certificate_names: ['Ada Lovelace'],
			operator_pass_terms_version: '2026-10-31',
			version: 1,
		} as OperatorPassPurchaseRow;
		await repository.createPurchase(purchase);
		const certificates = new OperatorPassCertificateService(repository, deps.cacheService, deps.kvClient);
		await certificates.issueForPurchase(purchase);
		const passId = `${purchase.checkout_session_id}:1`;
		await certificates.reissuePage({passId, holderName: 'Ada King', reason: 'name_change'});
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		const data = z
			.object({
				operator_pass_certificates: z.array(
					z.object({version: z.number(), holder_name: z.string().nullable(), pdf_path: z.string().nullable()}),
				),
			})
			.parse(archive.readJson('user.json'));
		expect(data.operator_pass_certificates.map(({version, holder_name}) => [version, holder_name])).toEqual([
			[1, 'Ada Lovelace'],
			[2, 'Ada King'],
		]);
		for (const {pdf_path} of data.operator_pass_certificates) {
			expect(archive.entries[pdf_path!]?.length).toBeGreaterThan(0);
		}

		await certificates.reissuePage({passId, holderName: 'Grace Hopper', reason: 'objection'});
		const redacted = z
			.object({
				operator_pass_certificates: z.array(
					z.object({version: z.number(), holder_name: z.string().nullable(), pdf_path: z.string().nullable()}),
				),
			})
			.parse((await harvestUserArchive(harness, deps, AUTHOR)).readJson('user.json'));
		expect(
			redacted.operator_pass_certificates.map(({holder_name, pdf_path}) => [holder_name, pdf_path != null]),
		).toEqual([
			[null, false],
			[null, false],
			['Grace Hopper', true],
		]);
	});

	it('leaves the recovery kit out when the account has none', async () => {
		const data = await exportUserData({});
		expect('recovery_kit' in data).toBe(false);
	});
});

describe('harvested user settings', () => {
	it.each([
		{name: 'no custom status', customStatus: null, expected: null},
		{
			name: 'complete custom status',
			customStatus: {
				text: 'Away for lunch',
				emoji_id: createEmojiID(4000000000000000000n),
				emoji_name: 'lunch',
				emoji_animated: true,
				expires_at: new Date('2027-01-02T03:04:05.000Z'),
			},
			expected: {
				text: 'Away for lunch',
				emoji_id: '4000000000000000000',
				emoji_name: 'lunch',
				emoji_animated: true,
				expires_at: '2027-01-02T03:04:05.000Z',
			},
		},
		{
			name: 'nullable custom status fields',
			customStatus: {text: null, emoji_id: null, emoji_name: null, emoji_animated: false, expires_at: null},
			expected: {text: null, emoji_id: null, emoji_name: null, emoji_animated: false, expires_at: null},
		},
	])('writes the complete settings document with $name', async ({customStatus, expected}) => {
		await deps.userRepository.upsertSettings({
			user_id: AUTHOR,
			locale: 'sv-SE',
			theme: 'dark',
			status: 'dnd',
			status_resets_at: null,
			status_resets_to: null,
			custom_status: customStatus,
			developer_mode: true,
			message_display_compact: true,
			animate_emoji: false,
			animate_stickers: 2,
			gif_auto_play: true,
			render_embeds: true,
			render_reactions: false,
			render_spoilers: 1,
			inline_attachment_media: true,
			inline_embed_media: false,
			explicit_content_filter: 2,
			friend_source_flags: 3,
			incoming_call_flags: 0,
			group_dm_add_permission_flags: 0,
			default_guilds_restricted: true,
			bot_default_guilds_restricted: false,
			restricted_guilds: new Set([createGuildID(2000000000000000000n)]),
			bot_restricted_guilds: new Set([createGuildID(3000000000000000000n)]),
			guild_positions: [createGuildID(3000000000000000000n), createGuildID(2000000000000000000n)],
			guild_folders: [
				{
					folder_id: 4,
					name: 'Notes',
					color: 42,
					flags: 1,
					icon: 'folder',
					guild_ids: [createGuildID(3000000000000000000n)],
				},
			],
			afk_timeout: 300,
			time_format: 1,
			trusted_domains: new Set(['example.com']),
			default_hide_muted_channels: false,
			sensitive_content_friend_dm_filter: 0,
			sensitive_content_non_friend_dm_filter: 0,
			sensitive_content_guild_filter: 0,
			suppress_unprivileged_self_mentions: true,
			suppress_unprivileged_self_mentions_bypass_user_ids: new Set([createUserID(6000000000000000000n)]),
			staff_dm_access_user_ids: new Set([createUserID(7000000000000000000n)]),
			synced_preferences: null,
			profile_privacy: 2,
			default_share_voice_activity: false,
			privacy_setup_version: 3,
			privacy_setup_completed_at: new Date('2026-10-01T12:00:00.000Z'),
			version: 1,
		});
		const archive = await harvestUserArchive(harness, deps, AUTHOR);
		const {user_settings} = z.object({user_settings: z.unknown()}).parse(archive.readJson('user.json'));
		expect(user_settings).toEqual({
			locale: 'sv-SE',
			theme: 'dark',
			status: 'dnd',
			custom_status: expected,
			developer_mode: true,
			message_display_compact: true,
			animate_emoji: false,
			animate_stickers: 2,
			gif_auto_play: true,
			render_embeds: true,
			render_reactions: false,
			render_spoilers: 1,
			inline_attachment_media: true,
			inline_embed_media: false,
			explicit_content_filter: 2,
			friend_source_flags: 3,
			default_guilds_restricted: true,
			bot_default_guilds_restricted: false,
			restricted_guilds: ['2000000000000000000'],
			bot_restricted_guilds: ['3000000000000000000'],
			guild_positions: ['3000000000000000000', '2000000000000000000'],
			guild_folders: [{id: 4, name: 'Notes', color: 42, flags: 1, icon: 'folder', guild_ids: ['3000000000000000000']}],
			afk_timeout: 300,
			time_format: 1,
			suppress_unprivileged_self_mentions: true,
			suppress_unprivileged_self_mentions_bypass_user_ids: ['6000000000000000000'],
			staff_dm_access_user_ids: ['7000000000000000000'],
			synced_preferences: {},
			profile_privacy: 2,
			privacy_setup_version: 3,
			privacy_setup_completed_at: '2026-10-01T12:00:00.000Z',
		});
	});
});
