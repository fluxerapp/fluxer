// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import type {ActionEnvelope, ActionOutcome} from '@app/api/infrastructure/activity/Contract.generated';
import {User} from '@app/api/models/User';
import {NEW_CONVERSATION_LIMIT_MAX_MS} from '@app/api/user/NewConversationLimit';
import type {AccountStateDeps} from '@app/api/user/services/AccountStateApplier';
import {type AccountActionDeps, applyAction, handleActionMessage} from '@app/api/worker/AccountActionConsumer';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {AdminArchiveResponse} from '@fluxer/schema/src/domains/admin/AdminArchiveSchemas';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import type {JsMsg} from '@nats-io/jetstream';
import {beforeEach, describe, expect, it} from 'vitest';

const USER_ID = '1174109840998400001';
const NOW = 1_759_000_000_000;
const DAY_MS = 86_400_000;
const DELETION_DELAY_MS = 7 * DAY_MS;

function archiveAt(
	requestedAtMs: number,
	userId: UserID,
	requestedBy: UserID,
	archiveId: string,
	failed = false,
): AdminArchiveResponse {
	return {
		archive_id: archiveId,
		subject_type: 'user',
		subject_id: userId.toString(),
		requested_by: requestedBy.toString(),
		requested_at: new Date(requestedAtMs).toISOString(),
		started_at: null,
		completed_at: null,
		failed_at: failed ? new Date(requestedAtMs + 1_000).toISOString() : null,
		file_size: null,
		progress_percent: 0,
		progress_step: 'Queued',
		error_message: null,
		download_url_expires_at: null,
		expires_at: null,
	};
}

function limitEnvelope(overrides: Partial<Extract<ActionEnvelope, {type: 'set_account_limit'}>> = {}) {
	return envelope<'set_account_limit'>({
		type: 'set_account_limit',
		user_id: USER_ID,
		on: true,
		...overrides,
	});
}

function envelope<T extends ActionEnvelope['type']>(
	body: Omit<Extract<ActionEnvelope, {type: T}>, 'v' | 'id' | 'key' | 'issued_at_ms' | 'expires_at_ms'> &
		Partial<ActionEnvelope>,
): Extract<ActionEnvelope, {type: T}> {
	return {
		v: 1,
		id: 'a:07:4242:0',
		key: USER_ID,
		issued_at_ms: NOW,
		expires_at_ms: NOW + 60_000,
		...body,
	} as Extract<ActionEnvelope, {type: T}>;
}

class FakeUsers {
	readonly rows = new Map<string, UserRow>();
	casFailures = 0;

	put(overrides: Partial<UserRow> = {}): void {
		const row: UserRow = {...EMPTY_USER_ROW, user_id: createUserID(BigInt(USER_ID)), flags: 0n, ...overrides};
		this.rows.set(row.user_id.toString(), row);
	}

	current(): User {
		return new User(this.rows.get(USER_ID)!);
	}

	async findUnique(userId: UserID): Promise<User | null> {
		const row = this.rows.get(userId.toString());
		return row ? new User(row) : null;
	}

	async compareAndSetFlags(user: User, flags: bigint): Promise<User | null> {
		const row = this.rows.get(user.id.toString())!;
		if (this.casFailures > 0) {
			this.casFailures--;
			return null;
		}
		if ((row.flags ?? 0n) !== user.flags) return null;
		const next = {...row, flags};
		this.rows.set(user.id.toString(), next);
		return new User(next);
	}

	async patchUpsert(userId: UserID, patch: Partial<UserRow>): Promise<User> {
		const next = {...this.rows.get(userId.toString())!, ...patch};
		this.rows.set(userId.toString(), next);
		return new User(next);
	}
}

interface Harness {
	users: FakeUsers;
	cached: Map<string, unknown>;
	presence: Array<unknown>;
	profiles: Array<unknown>;
	bans: Array<{ip: string; ttl: number}>;
	refreshes: number;
	authored: Array<{channelId: ChannelID; messageId: MessageID}>;
	visibility: Array<UserID>;
	removed: Array<{channelId: ChannelID; authorId: UserID; messageIds: Array<MessageID>}>;
	archives: Array<AdminArchiveResponse>;
	archiveRequests: Array<{userId: UserID; requestedBy: UserID; includeAttachments: boolean}>;
	queued: Map<string, number>;
	deps: AccountActionDeps;
}

function harness(): Harness {
	const users = new FakeUsers();
	const h: Harness = {
		users,
		cached: new Map(),
		presence: [],
		profiles: [],
		bans: [],
		refreshes: 0,
		authored: [],
		visibility: [],
		removed: [],
		archives: [],
		archiveRequests: [],
		queued: new Map(),
		deps: null as never,
	};
	const authored = {
		listMessagesByAuthor: async (_authorId: UserID, limit: number, before?: MessageID) =>
			[...h.authored]
				.sort((a, b) => (a.messageId > b.messageId ? -1 : 1))
				.filter((m) => before === undefined || m.messageId < before)
				.slice(0, limit),
	};
	const state: AccountStateDeps = {
		users: users as unknown as AccountStateDeps['users'],
		dispatch: {
			userUpdated: async (user) => {
				h.presence.push(user.id);
			},
			profileChanged: async (user) => {
				h.profiles.push(user.id);
			},
			contentVisibilityChanged: async (user) => {
				h.visibility.push(user.id);
			},
			messagesRemoved: async (channelId, authorId, messageIds) => {
				h.removed.push({channelId, authorId, messageIds});
			},
		},
		ipBans: {
			isIpBanned: async () => false,
			banIpTemp: async (ip: string, ttl: number) => {
				h.bans.push({ip, ttl});
			},
		},
		cache: {
			publish: async () => {
				h.refreshes++;
			},
			get: async (key: string) => h.cached.get(key) ?? null,
			set: async (key: string, value: unknown) => {
				h.cached.set(key, value);
			},
			delete: async (key: string) => {
				h.cached.delete(key);
			},
		} as unknown as AccountStateDeps['cache'],
		archives: {
			listArchives: async ({subjectId}) =>
				h.archives.filter((archive) => archive.subject_id === subjectId?.toString()).reverse(),
			triggerUserArchive: async (userId, requestedBy, includeAttachments) => {
				h.archiveRequests.push({userId, requestedBy, includeAttachments});
				const archive = archiveAt(NOW, userId, requestedBy, String(9000 + h.archives.length));
				h.archives.push(archive);
				return archive;
			},
		},
		messageDeletionQueue: {
			scheduleDeletion: async (userId, scheduledAt) => {
				h.queued.set(userId.toString(), scheduledAt.getTime());
			},
			removeFromQueue: async (userId) => {
				h.queued.delete(userId.toString());
			},
		},
		messageDeletionDelayMs: DELETION_DELAY_MS,
		authored: authored as unknown as AccountStateDeps['authored'],
		now: () => NOW,
	};
	h.deps = {js: {} as AccountActionDeps['js'], state, now: () => NOW};
	return h;
}

describe('account action apply', () => {
	let h: Harness;
	beforeEach(() => {
		h = harness();
		h.users.put();
	});

	it('limits the account and reports what it observed', async () => {
		const outcome = await applyAction(h.deps, limitEnvelope());
		expect(outcome).toEqual({
			action_id: 'a:07:4242:0',
			action_type: 'set_account_limit',
			status: 'applied',
			detail: null,
			observed: {flags: UserFlags.ACCOUNT_LIMITED.toString(), deleted: false},
			user_id: USER_ID,
		});
		expect(h.users.current().flags).toBe(UserFlags.ACCOUNT_LIMITED);
		expect(h.presence).toHaveLength(1);
	});

	it('is a noop when the limitation already holds, so a redelivery changes nothing', async () => {
		await applyAction(h.deps, limitEnvelope());
		const second = await applyAction(h.deps, limitEnvelope());
		expect(second.status).toBe('noop');
		expect(h.presence).toHaveLength(1);
	});

	it('lifts the limitation and keeps the other flags', async () => {
		h.users.put({flags: UserFlags.HAS_SESSION_STARTED | UserFlags.ACCOUNT_LIMITED});
		const lift = limitEnvelope({on: false});
		expect((await applyAction(h.deps, lift)).status).toBe('applied');
		expect(h.users.current().flags).toBe(UserFlags.HAS_SESSION_STARTED);
		expect((await applyAction(h.deps, lift)).status).toBe('noop');
		expect(h.presence).toHaveLength(1);
	});

	it('never limits staff, exempt or system accounts', async () => {
		for (const overrides of [{flags: UserFlags.STAFF}, {flags: UserFlags.LIMIT_EXEMPT}, {system: true}] satisfies Array<
			Partial<UserRow>
		>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, limitEnvelope())).status).toBe('exempt');
			expect(h.users.current().flags & UserFlags.ACCOUNT_LIMITED).toBe(0n);
		}
		expect(h.presence).toHaveLength(0);
	});

	it('rereads and retries when the compare and set loses a race', async () => {
		h.users.casFailures = 2;
		const outcome = await applyAction(h.deps, limitEnvelope());
		expect(outcome.status).toBe('applied');
	});

	it('gives up after repeated compare and set losses', async () => {
		h.users.casFailures = 3;
		await expect(applyAction(h.deps, limitEnvelope())).rejects.toThrow();
		expect(h.users.current().flags).toBe(0n);
	});

	it('treats missing, deleted and bot accounts as ineligible', async () => {
		h.users.rows.clear();
		const missing = await applyAction(h.deps, limitEnvelope());
		expect(missing).toMatchObject({status: 'ineligible', observed: null});
		h.users.put({bot: true});
		const bot = await applyAction(h.deps, limitEnvelope());
		expect(bot.status).toBe('ineligible');
		h.users.put({flags: UserFlags.DELETED});
		const deleted = await applyAction(h.deps, limitEnvelope());
		expect(deleted).toMatchObject({status: 'ineligible', observed: {deleted: true}});
	});

	it('limits new conversations until the requested time and lifts the limit once', async () => {
		const on = envelope<'limit_new_conversations'>({
			type: 'limit_new_conversations',
			user_id: USER_ID,
			on: true,
			until_ms: NOW + 86_400_000,
		});
		expect(await applyAction(h.deps, on)).toMatchObject({status: 'applied', user_id: USER_ID});
		expect([...h.cached.values()]).toEqual([{until_ms: NOW + 86_400_000, applied_at_ms: NOW}]);
		expect((await applyAction(h.deps, on)).status).toBe('noop');
		const off = envelope<'limit_new_conversations'>({
			type: 'limit_new_conversations',
			user_id: USER_ID,
			on: false,
			until_ms: NOW,
		});
		expect((await applyAction(h.deps, off)).status).toBe('applied');
		expect(h.cached.size).toBe(0);
		expect((await applyAction(h.deps, off)).status).toBe('noop');
	});

	it('caps a limit at the maximum duration and skips one that already ended', async () => {
		const far = envelope<'limit_new_conversations'>({
			type: 'limit_new_conversations',
			user_id: USER_ID,
			on: true,
			until_ms: NOW + 2 * NEW_CONVERSATION_LIMIT_MAX_MS,
		});
		expect((await applyAction(h.deps, far)).status).toBe('applied');
		expect([...h.cached.values()]).toMatchObject([{until_ms: NOW + NEW_CONVERSATION_LIMIT_MAX_MS}]);
		h.cached.clear();
		const past = envelope<'limit_new_conversations'>({
			type: 'limit_new_conversations',
			user_id: USER_ID,
			on: true,
			until_ms: NOW,
		});
		expect((await applyAction(h.deps, past)).status).toBe('noop');
		expect(h.cached.size).toBe(0);
	});

	it('never limits staff, trusted, bot or deleted accounts', async () => {
		const on = envelope<'limit_new_conversations'>({
			type: 'limit_new_conversations',
			user_id: USER_ID,
			on: true,
			until_ms: NOW + 86_400_000,
		});
		for (const overrides of [{flags: UserFlags.STAFF}, {flags: UserFlags.LIMIT_EXEMPT}] satisfies Array<
			Partial<UserRow>
		>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, on)).status).toBe('exempt');
		}
		for (const overrides of [{bot: true}, {flags: UserFlags.DELETED}] satisfies Array<Partial<UserRow>>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, on)).status).toBe('ineligible');
		}
		expect(h.cached.size).toBe(0);
	});

	it('ignores unknown envelope fields', async () => {
		h.users.put({flags: UserFlags.HAS_SESSION_STARTED});
		const action = {...limitEnvelope(), unknown_field: true};
		expect((await applyAction(h.deps, action)).status).toBe('applied');
		expect(h.users.current().flags).toBe(UserFlags.HAS_SESSION_STARTED | UserFlags.ACCOUNT_LIMITED);
	});

	it('bans a public address until the requested time and refreshes the ban caches', async () => {
		const outcome = await applyAction(
			h.deps,
			envelope<'temp_ban_ip'>({type: 'temp_ban_ip', ip: '93.184.216.34', until_ms: NOW + 3_600_000}),
		);
		expect(outcome.status).toBe('applied');
		expect(h.bans).toEqual([{ip: '93.184.216.34', ttl: 3600}]);
		expect(h.refreshes).toBe(1);
	});

	it('bans an IPv6 client by its /64 same-IP key or by its address', async () => {
		const byKey = await applyAction(
			h.deps,
			envelope<'temp_ban_ip'>({type: 'temp_ban_ip', ip: '2001:4860:1:2::/64', until_ms: NOW + 3_600_000}),
		);
		expect(byKey.status).toBe('applied');
		const loose = await applyAction(
			h.deps,
			envelope<'temp_ban_ip'>({type: 'temp_ban_ip', ip: '2001:4860:1:2::/48', until_ms: NOW + 3_600_000}),
		);
		expect(loose.status).toBe('exempt');
		expect(h.bans).toEqual([{ip: '2001:4860:1:2::/64', ttl: 3600}]);
	});

	it('never bans a private address and skips bans about to end', async () => {
		const privateIp = await applyAction(
			h.deps,
			envelope<'temp_ban_ip'>({type: 'temp_ban_ip', ip: '10.0.0.1', until_ms: NOW + 3_600_000}),
		);
		expect(privateIp.status).toBe('exempt');
		const ending = await applyAction(
			h.deps,
			envelope<'temp_ban_ip'>({type: 'temp_ban_ip', ip: '93.184.216.34', until_ms: NOW + 30_000}),
		);
		expect(ending.status).toBe('noop');
		expect(h.bans).toEqual([]);
	});

	it('hides the profile reversibly and tells clients both ways', async () => {
		h.users.put({flags: UserFlags.ACCOUNT_LIMITED});
		const hide = envelope<'hide_profile'>({type: 'hide_profile', user_id: USER_ID, on: true});
		expect(await applyAction(h.deps, hide)).toMatchObject({status: 'applied', action_type: 'hide_profile'});
		expect(h.users.current().flags).toBe(UserFlags.ACCOUNT_LIMITED | UserFlags.PROFILE_HIDDEN);
		expect((await applyAction(h.deps, hide)).status).toBe('noop');
		const show = envelope<'hide_profile'>({type: 'hide_profile', user_id: USER_ID, on: false});
		expect((await applyAction(h.deps, show)).status).toBe('applied');
		expect(h.users.current().flags).toBe(UserFlags.ACCOUNT_LIMITED);
		expect((await applyAction(h.deps, show)).status).toBe('noop');
		expect(h.presence).toHaveLength(2);
		expect(h.profiles).toHaveLength(2);
	});

	it('never hides staff or trusted profiles and skips bots and deleted accounts', async () => {
		const hide = envelope<'hide_profile'>({type: 'hide_profile', user_id: USER_ID, on: true});
		for (const flags of [UserFlags.STAFF, UserFlags.LIMIT_EXEMPT]) {
			h.users.put({flags});
			expect((await applyAction(h.deps, hide)).status).toBe('exempt');
		}
		for (const overrides of [{bot: true}, {flags: UserFlags.DELETED}] satisfies Array<Partial<UserRow>>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, hide)).status).toBe('ineligible');
		}
		h.users.put({flags: UserFlags.STAFF | UserFlags.PROFILE_HIDDEN});
		const show = envelope<'hide_profile'>({type: 'hide_profile', user_id: USER_ID, on: false});
		expect((await applyAction(h.deps, show)).status).toBe('applied');
		expect(h.profiles).toHaveLength(1);
	});

	function hideEnvelope(on: boolean, sinceMs = NOW - 86_400_000) {
		return envelope<'hide_recent_messages'>({type: 'hide_recent_messages', user_id: USER_ID, since_ms: sinceMs, on});
	}

	function authorAt(timestampMs: number, channel: number): MessageID {
		const messageId = createMessageID(createSnowflakeFromTimestamp(timestampMs) + BigInt(h.authored.length));
		h.authored.push({channelId: createChannelID(BigInt(channel)), messageId});
		return messageId;
	}

	function removedIds(): Array<MessageID> {
		return h.removed.flatMap((entry) => entry.messageIds).sort((a, b) => (a < b ? -1 : 1));
	}

	it('hides every message from the window onward and tells each channel they are gone', async () => {
		const since = NOW - 86_400_000;
		const before = [authorAt(since - 60_000, 100), authorAt(since - 1, 101)];
		const inside: Array<MessageID> = [authorAt(since, 100)];
		for (let i = 0; i < 250; i++) inside.push(authorAt(since + 1_000 + i, 100 + (i % 3)));
		const outcome = await applyAction(h.deps, hideEnvelope(true, since));
		expect(outcome).toEqual({
			action_id: 'a:07:4242:0',
			action_type: 'hide_recent_messages',
			status: 'applied',
			detail: 'messages=251',
			observed: {flags: '0', deleted: false},
			user_id: USER_ID,
		});
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(since);
		expect(h.visibility).toEqual([createUserID(BigInt(USER_ID))]);
		expect(removedIds()).toEqual([...inside].sort((a, b) => (a < b ? -1 : 1)));
		for (const id of before) expect(removedIds()).not.toContain(id);
		for (const entry of h.removed) {
			expect(entry.authorId).toBe(createUserID(BigInt(USER_ID)));
			for (const id of entry.messageIds) {
				expect(h.authored.find((m) => m.messageId === id)?.channelId).toBe(entry.channelId);
			}
		}
		expect(h.users.current().flags).toBe(0n);
		expect(h.queued.size).toBe(0);
	});

	it('keeps the wider window when asked again and resends the deletes as a noop', async () => {
		const since = NOW - 86_400_000;
		authorAt(since + 5_000, 100);
		await applyAction(h.deps, hideEnvelope(true, since));
		const later = await applyAction(h.deps, hideEnvelope(true, since + 60_000));
		expect(later).toMatchObject({status: 'noop', detail: 'messages=1'});
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(since);
		expect(h.visibility).toHaveLength(1);
		const wider = await applyAction(h.deps, hideEnvelope(true, since - 60_000));
		expect(wider.status).toBe('applied');
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(since - 60_000);
		expect(h.visibility).toHaveLength(2);
	});

	it('reports a hide with nothing in the window as applied with no deletes', async () => {
		authorAt(NOW - 2 * 86_400_000, 100);
		const outcome = await applyAction(h.deps, hideEnvelope(true));
		expect(outcome).toMatchObject({status: 'applied', detail: 'messages=0'});
		expect(h.removed).toEqual([]);
	});

	it('restores by clearing the window and sends no gateway events', async () => {
		const since = NOW - 86_400_000;
		authorAt(since + 5_000, 100);
		h.users.put({content_hidden_since: new Date(since)});
		const outcome = await applyAction(h.deps, hideEnvelope(false));
		expect(outcome).toMatchObject({status: 'applied', detail: null, observed: {flags: '0', deleted: false}});
		expect(h.users.current().contentHiddenSince).toBeNull();
		expect(h.removed).toEqual([]);
		expect(h.presence).toHaveLength(0);
		expect(h.visibility).toHaveLength(1);
		expect((await applyAction(h.deps, hideEnvelope(false))).status).toBe('noop');
		expect(h.visibility).toHaveLength(1);
	});

	it('never hides staff, trusted or system messages and skips bots and deleted accounts', async () => {
		authorAt(NOW - 1_000, 100);
		for (const overrides of [{flags: UserFlags.STAFF}, {flags: UserFlags.LIMIT_EXEMPT}, {system: true}] satisfies Array<
			Partial<UserRow>
		>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, hideEnvelope(true))).status).toBe('exempt');
			expect(h.users.current().contentHiddenSince).toBeNull();
		}
		for (const overrides of [{bot: true}, {flags: UserFlags.DELETED}] satisfies Array<Partial<UserRow>>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, hideEnvelope(true))).status).toBe('ineligible');
		}
		h.users.rows.clear();
		expect(await applyAction(h.deps, hideEnvelope(true))).toMatchObject({status: 'ineligible', observed: null});
		expect(h.removed).toEqual([]);
		expect(h.visibility).toEqual([]);
		h.users.put({flags: UserFlags.STAFF, content_hidden_since: new Date(NOW - 1_000)});
		expect((await applyAction(h.deps, hideEnvelope(false))).status).toBe('applied');
		expect(h.users.current().contentHiddenSince).toBeNull();
	});

	it('keeps the message hide and the profile mask independent', async () => {
		const since = NOW - 86_400_000;
		authorAt(since + 5_000, 100);
		await applyAction(h.deps, hideEnvelope(true, since));
		await applyAction(h.deps, envelope<'hide_profile'>({type: 'hide_profile', user_id: USER_ID, on: true}));
		expect(h.users.current().flags).toBe(UserFlags.PROFILE_HIDDEN);
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(since);
		await applyAction(h.deps, hideEnvelope(false));
		expect(h.users.current().flags).toBe(UserFlags.PROFILE_HIDDEN);
		expect(h.users.current().contentHiddenSince).toBeNull();
		await applyAction(h.deps, hideEnvelope(true, since));
		await applyAction(h.deps, envelope<'hide_profile'>({type: 'hide_profile', user_id: USER_ID, on: false}));
		expect(h.users.current().flags).toBe(0n);
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(since);
	});

	it('hides every message the account ever sent when the window starts at zero', async () => {
		const all = [authorAt(1_420_070_401_000, 100), authorAt(NOW - 400 * DAY_MS, 101), authorAt(NOW - 1_000, 102)];
		const outcome = await applyAction(h.deps, hideEnvelope(true, 0));
		expect(outcome).toMatchObject({status: 'applied', detail: 'messages=3'});
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(0);
		expect(removedIds()).toEqual([...all].sort((a, b) => (a < b ? -1 : 1)));
		expect((await applyAction(h.deps, hideEnvelope(true, NOW - DAY_MS))).status).toBe('noop');
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(0);
		expect((await applyAction(h.deps, hideEnvelope(false, 0))).status).toBe('applied');
		expect(h.users.current().contentHiddenSince).toBeNull();
	});

	it('hides the messages of an account whose deletion is only scheduled', async () => {
		authorAt(NOW - 1_000, 100);
		h.users.put({flags: UserFlags.DELETED | UserFlags.SPAMMER, pending_deletion_at: new Date(NOW + 60 * DAY_MS)});
		expect((await applyAction(h.deps, hideEnvelope(true, 0))).status).toBe('applied');
		expect(h.users.current().contentHiddenSince?.getTime()).toBe(0);
	});

	function purgeEnvelope(on = true) {
		return envelope<'delete_user_messages'>({type: 'delete_user_messages', user_id: USER_ID, on});
	}

	const userId = createUserID(BigInt(USER_ID));

	it('archives with attachments and schedules the deletion instead of deleting now', async () => {
		authorAt(NOW - 1_000, 100);
		const outcome = await applyAction(h.deps, purgeEnvelope());
		const scheduledAt = NOW + DELETION_DELAY_MS;
		expect(outcome).toEqual({
			action_id: 'a:07:4242:0',
			action_type: 'delete_user_messages',
			status: 'applied',
			detail: `archive=9000 reused=false scheduled_at=${new Date(scheduledAt).toISOString()}`,
			observed: {flags: '0', deleted: false},
			user_id: USER_ID,
		});
		expect(h.archiveRequests).toEqual([{userId, requestedBy: SYSTEM_USER_ID, includeAttachments: true}]);
		expect(h.users.current().pendingBulkMessageDeletionAt?.getTime()).toBe(scheduledAt);
		expect(h.queued.get(USER_ID)).toBe(scheduledAt);
		expect(h.removed).toEqual([]);
		expect(h.authored).toHaveLength(1);
		expect(h.presence).toEqual([userId]);
	});

	it('reuses an archive from the last day and makes a new one when it is older or failed', async () => {
		h.archives.push(archiveAt(NOW - DAY_MS + 60_000, userId, createUserID(42n), '777'));
		expect((await applyAction(h.deps, purgeEnvelope())).detail).toMatch(/^archive=777 reused=true /);
		expect(h.archiveRequests).toEqual([]);
		h.archives.length = 0;
		h.archives.push(archiveAt(NOW - DAY_MS - 60_000, userId, createUserID(42n), '778'));
		h.archives.push(archiveAt(NOW - 60_000, userId, createUserID(42n), '779', true));
		h.users.put();
		expect((await applyAction(h.deps, purgeEnvelope())).detail).toMatch(/^archive=9002 reused=false /);
		expect(h.archiveRequests).toHaveLength(1);
	});

	it('keeps an earlier schedule and pulls a later one forward', async () => {
		const earlier = NOW + DAY_MS;
		h.users.put({pending_bulk_message_deletion_at: new Date(earlier), pending_bulk_message_deletion_message_count: 4});
		const kept = await applyAction(h.deps, purgeEnvelope());
		expect(kept).toMatchObject({status: 'noop'});
		expect(kept.detail).toContain(`scheduled_at=${new Date(earlier).toISOString()}`);
		expect(h.users.current().pendingBulkMessageDeletionAt?.getTime()).toBe(earlier);
		expect(h.users.current().pendingBulkMessageDeletionMessageCount).toBe(4);
		expect(h.queued.get(USER_ID)).toBe(earlier);
		expect(h.presence).toHaveLength(0);
		h.users.put({pending_bulk_message_deletion_at: new Date(NOW + 30 * DAY_MS)});
		expect((await applyAction(h.deps, purgeEnvelope())).status).toBe('applied');
		expect(h.users.current().pendingBulkMessageDeletionAt?.getTime()).toBe(NOW + DELETION_DELAY_MS);
		expect(h.queued.get(USER_ID)).toBe(NOW + DELETION_DELAY_MS);
		expect(h.archiveRequests).toHaveLength(1);
	});

	it('schedules for accounts already scheduled for deletion and paid accounts', async () => {
		for (const overrides of [
			{flags: UserFlags.DELETED | UserFlags.SPAMMER, pending_deletion_at: new Date(NOW + 60 * DAY_MS)},
			{has_ever_purchased: true, premium_type: 2},
		] satisfies Array<Partial<UserRow>>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, purgeEnvelope())).status).toBe('applied');
			expect(h.users.current().pendingBulkMessageDeletionAt?.getTime()).toBe(NOW + DELETION_DELAY_MS);
		}
	});

	it('cancels the schedule on off and is a noop when nothing is scheduled', async () => {
		await applyAction(h.deps, purgeEnvelope());
		const cancelled = await applyAction(h.deps, purgeEnvelope(false));
		expect(cancelled).toMatchObject({status: 'applied', detail: null, observed: {flags: '0', deleted: false}});
		expect(h.users.current().pendingBulkMessageDeletionAt).toBeNull();
		expect(h.queued.has(USER_ID)).toBe(false);
		expect(h.presence).toHaveLength(2);
		expect((await applyAction(h.deps, purgeEnvelope(false))).status).toBe('noop');
		expect(h.archives).toHaveLength(1);
		h.users.put({flags: UserFlags.STAFF, pending_bulk_message_deletion_at: new Date(NOW + DAY_MS)});
		expect((await applyAction(h.deps, purgeEnvelope(false))).status).toBe('applied');
		h.users.rows.clear();
		expect(await applyAction(h.deps, purgeEnvelope(false))).toMatchObject({status: 'ineligible', observed: null});
	});

	it('never schedules for staff, trusted, system, bot, deleted or missing accounts', async () => {
		for (const overrides of [{flags: UserFlags.STAFF}, {flags: UserFlags.LIMIT_EXEMPT}, {system: true}] satisfies Array<
			Partial<UserRow>
		>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, purgeEnvelope())).status).toBe('exempt');
		}
		for (const overrides of [{bot: true}, {flags: UserFlags.DELETED}] satisfies Array<Partial<UserRow>>) {
			h.users.put(overrides);
			expect((await applyAction(h.deps, purgeEnvelope())).status).toBe('ineligible');
		}
		h.users.rows.clear();
		expect(await applyAction(h.deps, purgeEnvelope())).toMatchObject({status: 'ineligible', observed: null});
		expect(h.archiveRequests).toEqual([]);
		expect(h.queued.size).toBe(0);
	});

	it('answers expired actions and unknown shapes without touching the account', async () => {
		const expired = await applyAction(h.deps, limitEnvelope({expires_at_ms: NOW}));
		expect(expired.status).toBe('expired');
		const future = await applyAction(h.deps, {...limitEnvelope(), v: 2});
		expect(future.status).toBe('unsupported');
		const unknown = await applyAction(h.deps, {...limitEnvelope(), type: 'future_type'} as unknown as ActionEnvelope);
		expect(unknown).toMatchObject({status: 'unsupported', action_type: 'future_type'});
		expect(h.users.current().flags).toBe(0n);
	});
});

interface FakeMsgState {
	acked: number;
	naks: Array<number | undefined>;
	terms: number;
}

function fakeMsg(data: string, deliveryCount: number): {msg: JsMsg; state: FakeMsgState} {
	const state: FakeMsgState = {acked: 0, naks: [], terms: 0};
	const msg = {
		data: new TextEncoder().encode(data),
		info: {deliveryCount},
		ackAck: async () => {
			state.acked++;
			return true;
		},
		nak: (delay?: number) => {
			state.naks.push(delay);
		},
		term: () => {
			state.terms++;
		},
	} as unknown as JsMsg;
	return {msg, state};
}

describe('account action messages', () => {
	it('publishes the outcome before acknowledging, and terminates undecodable envelopes', async () => {
		const h = harness();
		h.users.put();
		const outcomes: Array<ActionOutcome> = [];
		h.deps.publishOutcome = async (_key, outcome) => {
			outcomes.push(outcome);
		};
		const good = fakeMsg(JSON.stringify(limitEnvelope()), 1);
		await handleActionMessage(h.deps, good.msg);
		expect(outcomes.map((outcome) => outcome.status)).toEqual(['applied']);
		expect(good.state.acked).toBe(1);
		const bad = fakeMsg('{not json', 1);
		await handleActionMessage(h.deps, bad.msg);
		expect(bad.state.terms).toBe(1);
		expect(outcomes).toHaveLength(1);
	});

	it('retries a failing apply with backoff and reports failed after the last attempt', async () => {
		const h = harness();
		h.users.put();
		h.users.findUnique = async () => {
			throw new Error('datastore unavailable');
		};
		const outcomes: Array<ActionOutcome> = [];
		h.deps.publishOutcome = async (_key, outcome) => {
			outcomes.push(outcome);
		};
		const data = JSON.stringify(limitEnvelope());
		const early = fakeMsg(data, 3);
		await handleActionMessage(h.deps, early.msg);
		expect(early.state.naks).toEqual([3000]);
		expect(outcomes).toEqual([]);
		const last = fakeMsg(data, 10);
		await handleActionMessage(h.deps, last.msg);
		expect(outcomes).toMatchObject([{status: 'failed', detail: 'Error: datastore unavailable'}]);
		expect(last.state.acked).toBe(1);
	});
});
