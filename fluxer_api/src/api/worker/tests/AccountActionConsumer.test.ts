// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {
	ACTIONS_STREAM,
	type ActionEnvelope,
	type ActionOutcome,
	effectsConsumer,
} from '@app/api/infrastructure/activity/Contract.generated';
import {User} from '@app/api/models/User';
import type {AccountStateDeps} from '@app/api/user/services/AccountStateApplier';
import {
	type AccountActionDeps,
	applyAction,
	handleActionMessage,
	startAccountActionConsumer,
	stopAccountActionConsumer,
} from '@app/api/worker/AccountActionConsumer';
import {SuspiciousActivityFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {AckPolicy, DeliverPolicy, type JsMsg, jetstream, jetstreamManager} from '@nats-io/jetstream';
import {connect} from '@nats-io/transport-node';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const USER_ID = '1174109840998400001';
const NOW = 1_759_000_000_000;
const NATS_URL = process.env.FLUXER_TEST_ACTIVITY_NATS_URL;

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

	async patchUpsert(userId: UserID, patch: Partial<UserRow>): Promise<User> {
		const row = {...this.rows.get(userId.toString())!, ...patch};
		this.rows.set(userId.toString(), row);
		return new User(row);
	}

	async compareAndSetSuspiciousFlags(user: User, flags: number): Promise<User | null> {
		const row = this.rows.get(user.id.toString())!;
		if (this.casFailures > 0) {
			this.casFailures--;
			return null;
		}
		if ((row.suspicious_activity_flags ?? 0) !== user.suspiciousActivityFlags) return null;
		const next = {...row, suspicious_activity_flags: flags};
		this.rows.set(user.id.toString(), next);
		return new User(next);
	}
}

interface Harness {
	users: FakeUsers;
	presence: Array<unknown>;
	contactLogs: Array<unknown>;
	bans: Array<{ip: string; ttl: number}>;
	refreshes: number;
	deps: AccountActionDeps;
}

function harness(): Harness {
	const users = new FakeUsers();
	const h: Harness = {users, presence: [], contactLogs: [], bans: [], refreshes: 0, deps: null as never};
	const state: AccountStateDeps = {
		users: users as unknown as AccountStateDeps['users'],
		dispatch: {
			userUpdated: async (user) => {
				h.presence.push(user.id);
			},
			memberProfilesUpdated: async () => {},
		},
		contactChangeLog: {
			recordDiff: async (params) => {
				h.contactLogs.push(params);
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
		} as unknown as AccountStateDeps['cache'],
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

	it('sets requested suspicious bits and reports what it observed', async () => {
		const outcome = await applyAction(
			h.deps,
			envelope<'set_suspicious_flags'>({
				type: 'set_suspicious_flags',
				user_id: USER_ID,
				set: 1,
				clear: 0,
				if_current: 0,
			}),
		);
		expect(outcome).toEqual({
			action_id: 'a:07:4242:0',
			action_type: 'set_suspicious_flags',
			status: 'applied',
			detail: null,
			observed: {flags: '0', suspicious_flags: 1, has_verified_phone: false, deleted: false},
		});
		expect(h.users.current().suspiciousActivityFlags).toBe(1);
		expect(h.presence).toHaveLength(1);
	});

	it('is a noop when the bits already hold, so a redelivery changes nothing', async () => {
		const action = envelope<'set_suspicious_flags'>({
			type: 'set_suspicious_flags',
			user_id: USER_ID,
			set: 1,
			clear: 0,
			if_current: null,
		});
		await applyAction(h.deps, action);
		const second = await applyAction(h.deps, action);
		expect(second.status).toBe('noop');
		expect(h.presence).toHaveLength(1);
	});

	it('refuses a stale precondition and leaves the flags alone', async () => {
		h.users.put({suspicious_activity_flags: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL});
		const outcome = await applyAction(
			h.deps,
			envelope<'set_suspicious_flags'>({
				type: 'set_suspicious_flags',
				user_id: USER_ID,
				set: 1,
				clear: 0,
				if_current: 0,
			}),
		);
		expect(outcome.status).toBe('conflict');
		expect(outcome.observed?.suspicious_flags).toBe(SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL);
		expect(h.users.current().suspiciousActivityFlags).toBe(SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL);
	});

	it('rereads and retries when the compare and set loses a race', async () => {
		h.users.casFailures = 2;
		const outcome = await applyAction(
			h.deps,
			envelope<'set_suspicious_flags'>({
				type: 'set_suspicious_flags',
				user_id: USER_ID,
				set: 1,
				clear: 0,
				if_current: null,
			}),
		);
		expect(outcome.status).toBe('applied');
	});

	it('treats missing, deleted and bot accounts as ineligible', async () => {
		h.users.rows.clear();
		const missing = await applyAction(
			h.deps,
			envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true}),
		);
		expect(missing).toMatchObject({status: 'ineligible', observed: null});
		h.users.put({bot: true});
		const bot = await applyAction(h.deps, envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true}));
		expect(bot.status).toBe('ineligible');
		h.users.put({flags: UserFlags.DELETED});
		const deleted = await applyAction(
			h.deps,
			envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true}),
		);
		expect(deleted).toMatchObject({status: 'ineligible', observed: {deleted: true}});
	});

	it('sets and clears the SPAMMER flag idempotently', async () => {
		const on = envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true});
		expect((await applyAction(h.deps, on)).status).toBe('applied');
		expect(h.users.current().flags & UserFlags.SPAMMER).toBe(UserFlags.SPAMMER);
		expect((await applyAction(h.deps, on)).status).toBe('noop');
		const off = envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: false});
		expect((await applyAction(h.deps, off)).status).toBe('applied');
		expect(h.users.current().flags & UserFlags.SPAMMER).toBe(0n);
	});

	it('attaches a verified phone once and logs the contact change once', async () => {
		h.users.put({
			flags: UserFlags.SPAMMER,
			suspicious_activity_flags:
				SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
		});
		const action = envelope<'phone_verified'>({
			type: 'phone_verified',
			user_id: USER_ID,
			method: 'outbound',
			clear_suspicious: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
		});
		expect((await applyAction(h.deps, action)).status).toBe('applied');
		const user = h.users.current();
		expect(user.hasVerifiedPhone).toBe(true);
		expect(user.suspiciousActivityFlags).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);
		expect(user.flags & UserFlags.SPAMMER).toBe(UserFlags.SPAMMER);
		expect((await applyAction(h.deps, action)).status).toBe('noop');
		expect(h.contactLogs).toHaveLength(1);
	});

	it('ignores unknown envelope fields', async () => {
		h.users.put({
			flags: UserFlags.HAS_SESSION_STARTED,
			suspicious_activity_flags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
		});
		const action = {
			...envelope<'phone_verified'>({
				type: 'phone_verified',
				user_id: USER_ID,
				method: 'outbound',
				clear_suspicious: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
			}),
			unknown_field: true,
		};
		expect((await applyAction(h.deps, action)).status).toBe('applied');
		const user = h.users.current();
		expect(user.hasVerifiedPhone).toBe(true);
		expect(user.suspiciousActivityFlags).toBe(0);
		expect(user.flags).toBe(UserFlags.HAS_SESSION_STARTED);
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

	it('answers expired actions and unknown shapes without touching the account', async () => {
		const expired = await applyAction(
			h.deps,
			envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true, expires_at_ms: NOW}),
		);
		expect(expired.status).toBe('expired');
		const future = await applyAction(h.deps, {
			...envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true}),
			v: 2,
		});
		expect(future.status).toBe('unsupported');
		const unknown = await applyAction(h.deps, {
			...envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true}),
			type: 'future_type',
		} as unknown as ActionEnvelope);
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
		const good = fakeMsg(JSON.stringify(envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true})), 1);
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
		const data = JSON.stringify(envelope<'set_spammer'>({type: 'set_spammer', user_id: USER_ID, on: true}));
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

describe.skipIf(!NATS_URL)('account action consumer against JetStream', () => {
	let nc: Awaited<ReturnType<typeof connect>>;
	beforeAll(async () => {
		nc = await connect({servers: NATS_URL, token: process.env.FLUXER_TEST_ACTIVITY_NATS_TOKEN});
		const jsm = await jetstreamManager(nc);
		await jsm.streams.delete(ACTIONS_STREAM).catch(() => undefined);
		await jsm.streams.add({name: ACTIONS_STREAM, subjects: ['act.*']});
		for (const partition of [0, 7]) {
			await jsm.consumers.add(ACTIONS_STREAM, {
				durable_name: effectsConsumer(partition),
				filter_subject: `act.${String(partition).padStart(2, '0')}`,
				ack_policy: AckPolicy.Explicit,
				max_ack_pending: 1,
				deliver_policy: DeliverPolicy.All,
			});
		}
	});
	afterAll(async () => {
		await stopAccountActionConsumer();
		await nc?.close();
	});

	it('applies each action in order and acknowledges it after the outcome', async () => {
		const h = harness();
		h.users.put();
		h.deps.now = Date.now;
		const outcomes: Array<ActionOutcome> = [];
		h.deps.publishOutcome = async (_key, outcome) => {
			outcomes.push(outcome);
		};
		h.deps.js = jetstream(nc);
		h.deps.retryDelayMs = 200;
		const js = jetstream(nc);
		const expires = Date.now() + 60_000;
		await js.publish(
			'act.07',
			JSON.stringify(
				envelope<'set_spammer'>({
					type: 'set_spammer',
					id: 'a:07:1:0',
					user_id: USER_ID,
					on: true,
					expires_at_ms: expires,
				}),
			),
			{msgID: 'a:07:1:0'},
		);
		await js.publish(
			'act.07',
			JSON.stringify(
				envelope<'set_spammer'>({
					type: 'set_spammer',
					id: 'a:07:2:0',
					user_id: USER_ID,
					on: false,
					expires_at_ms: expires,
				}),
			),
			{msgID: 'a:07:2:0'},
		);
		startAccountActionConsumer(h.deps);
		const deadline = Date.now() + 10_000;
		while (outcomes.length < 2 && Date.now() < deadline) {
			await new Promise((resolve) => setTimeout(resolve, 50));
		}
		expect(outcomes.map((outcome) => [outcome.action_id, outcome.status])).toEqual([
			['a:07:1:0', 'applied'],
			['a:07:2:0', 'applied'],
		]);
		expect(h.users.current().flags & UserFlags.SPAMMER).toBe(0n);
		const info = await (await jetstreamManager(nc)).consumers.info(ACTIONS_STREAM, effectsConsumer(7));
		expect(info.num_ack_pending).toBe(0);
		expect(info.num_pending).toBe(0);
	});
});
