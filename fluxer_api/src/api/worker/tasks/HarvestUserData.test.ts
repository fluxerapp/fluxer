// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type GuildID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {syncChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import {ThreadMember} from '@app/api/models/ThreadMember';
import {User} from '@app/api/models/User';
import {mapStorePurchaseToResponse} from '@app/api/store_billing/StoreBillingMappers';
import {buildStorePurchaseRow} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {buildUserDataJson, collectThreadMemberships, harvestMessages} from '@app/api/worker/tasks/HarvestUserData';
import {ChannelThreadsConfigSchema} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {afterEach, describe, expect, it, vi} from 'vitest';

const tainted = vi.hoisted(() => new Set<string>());

vi.mock('@app/api/experiment/ChannelThreadsGate', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/api/experiment/ChannelThreadsGate')>()),
	isTainted: async (guildId: bigint) => tainted.has(guildId.toString()),
}));

const AUTHOR = createUserID(1000000000000000000n);

function makeRepository(refs: Array<{channelId: ChannelID; messageId: MessageID}>, missing = new Set<string>()) {
	let pages = 0;
	return {
		pages: () => pages,
		listMessagesByAuthor: async (_userId: typeof AUTHOR, limit: number, lastMessageId?: MessageID) => {
			pages++;
			const start = lastMessageId ? refs.findIndex((r) => r.messageId === lastMessageId) + 1 : 0;
			return refs.slice(start, start + limit);
		},
		getMessage: async (_channelId: ChannelID, messageId: MessageID) =>
			missing.has(messageId.toString()) ? null : {content: `body ${messageId.toString()}`, attachments: undefined},
	};
}

function refsAcross(channelCount: number, perChannel: number) {
	const out: Array<{channelId: ChannelID; messageId: MessageID}> = [];
	let id = 1500000000000000000n;
	for (let c = 0; c < channelCount; c++) {
		const channelId = createChannelID(2000000000000000000n + BigInt(c));
		for (let m = 0; m < perChannel; m++) {
			out.push({channelId, messageId: createMessageID(id)});
			id += 1n;
		}
	}
	return out;
}

describe('harvestMessages', () => {
	it('reads past a single page instead of stopping at one query', async () => {
		const refs = refsAcross(1, 2500);
		const repo = makeRepository(refs);
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.totalMessages).toBe(2500);
		expect(repo.pages()).toBeGreaterThan(1);
	});

	it('groups every message under its own channel', async () => {
		const repo = makeRepository(refsAcross(3, 4));
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.channelMessagesMap.size).toBe(3);
		for (const messages of result.channelMessagesMap.values()) {
			expect(messages).toHaveLength(4);
		}
		expect(result.totalMessages).toBe(12);
	});

	it('leaves out a message the repository cannot return', async () => {
		const refs = refsAcross(1, 5);
		const repo = makeRepository(refs, new Set([refs[2].messageId.toString()]));
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.totalMessages).toBe(4);
	});

	it('returns an empty map for an account with no messages', async () => {
		const repo = makeRepository([]);
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.totalMessages).toBe(0);
		expect(result.channelMessagesMap.size).toBe(0);
	});
});

function userDataParams(overrides: Partial<Parameters<typeof buildUserDataJson>[0]> = {}) {
	return {
		user: new User({...EMPTY_USER_ROW, user_id: AUTHOR, username: 'exporter', discriminator: 1}),
		userId: AUTHOR,
		productName: 'Fluxer',
		authSessions: [],
		relationships: [],
		userNotes: new Map(),
		userSettings: null,
		guildMemberships: [],
		guildSettings: [],
		savedMessages: [],
		privateChannels: [],
		favoriteMemes: [],
		pushSubscriptions: [],
		webAuthnCredentials: [],
		mfaBackupCodes: [],
		recoveryKitCreatedAt: null,
		createdGiftCodes: [],
		payments: [],
		storePurchases: [],
		threadMemberships: [],
		oauthClients: [],
		connections: [],
		pinnedDms: [],
		authorizedIps: [],
		activityData: {last_active_at: null, last_active_ip: null},
		...overrides,
	};
}

describe('buildUserDataJson', () => {
	it('exports the store purchases in their public shape', () => {
		const purchase = buildStorePurchaseRow({user_id: AUTHOR});
		const data = buildUserDataJson(userDataParams({storePurchases: [purchase]}));
		expect(data.store_purchases).toEqual([mapStorePurchaseToResponse(purchase)]);
	});

	it('exports when the recovery kit was created and nothing else about it', () => {
		const createdAt = new Date('2026-10-01T12:00:00.000Z');
		const data = buildUserDataJson(userDataParams({recoveryKitCreatedAt: createdAt}));
		expect(data.recovery_kit).toEqual({created_at: '2026-10-01T12:00:00.000Z'});
	});

	it('leaves the recovery kit out when the account has none', () => {
		const data = buildUserDataJson(userDataParams());
		expect('recovery_kit' in data).toBe(false);
	});
});

describe('collectThreadMemberships', () => {
	const guildId = createGuildID(1400000000000000000n);
	const threadId = createChannelID(1400000000000000010n);
	const member = new ThreadMember({
		thread_id: threadId,
		user_id: AUTHOR,
		guild_id: guildId,
		parent_id: createChannelID(1400000000000000001n),
		join_timestamp: new Date('2026-09-27T00:00:00.000Z'),
		flags: 1,
		muted: false,
		mute_config: null,
	});
	let reads = 0;
	const threads = {
		listJoinedThreadIds: async (_userId: UserID, id: GuildID) => {
			reads++;
			return id === guildId ? [threadId] : [];
		},
		getMember: async () => member,
	};
	const setEverEnabled = (everEnabled: boolean) =>
		syncChannelThreadsConfig(JSON.stringify({ever_enabled: everEnabled}), (raw) =>
			ChannelThreadsConfigSchema.parse(JSON.parse(raw ?? '{}')),
		);

	afterEach(() => {
		reads = 0;
		tainted.clear();
		syncChannelThreadsConfig(null, () => ChannelThreadsConfigSchema.parse({}));
	});

	it('reads nothing on an instance that never enabled threads', async () => {
		setEverEnabled(false);
		expect(await collectThreadMemberships(threads, AUTHOR, [guildId])).toEqual([]);
		expect(reads).toBe(0);
	});

	it('collects the joined thread memberships of every tainted guild and skips the rest', async () => {
		setEverEnabled(true);
		tainted.add(guildId.toString());
		const collected = await collectThreadMemberships(threads, AUTHOR, [guildId, createGuildID(1400000000000000002n)]);
		expect(collected).toEqual([member]);
		expect(reads).toBe(1);
	});
});
