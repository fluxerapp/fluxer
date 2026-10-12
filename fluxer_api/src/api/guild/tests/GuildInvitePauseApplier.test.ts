// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID, type GuildID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {
	applyPauseGuildInvites,
	GUILD_INVITE_PAUSE_SCHEDULE_KEY,
	type GuildInvitePauseDeps,
	liftExpiredGuildInvitePauses,
} from '@app/api/guild/services/GuildInvitePauseApplier';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {ActionEnvelope, Event} from '@app/api/infrastructure/activity/Contract.generated';
import {Guild} from '@app/api/models/Guild';
import {applyAction} from '@app/api/worker/AccountActionConsumer';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const GUILD_ID = '1472623911696138261';
const NOW = 1_760_000_000_000;
const HOUR_MS = 3_600_000;
const BASE_FEATURES = [GuildFeatures.BANNER];

type PauseEnvelope = Extract<ActionEnvelope, {type: 'pause_guild_invites'}>;

function pauseEnvelope(overrides: Partial<PauseEnvelope> = {}): PauseEnvelope {
	return {
		v: 1,
		id: 'a:11:guild:0',
		key: `guild:${GUILD_ID}`,
		issued_at_ms: NOW,
		expires_at_ms: NOW + 60_000,
		type: 'pause_guild_invites',
		guild_id: GUILD_ID,
		on: true,
		until_ms: NOW + 2 * HOUR_MS,
		...overrides,
	};
}

function guildRow(features: Iterable<string>, pausedUntil: Date | null = null): GuildRow {
	return {
		guild_id: createGuildID(BigInt(GUILD_ID)),
		owner_id: createUserID(1472623911696138262n),
		name: 'Paused',
		vanity_url_code: null,
		icon_hash: null,
		banner_hash: null,
		banner_width: null,
		banner_height: null,
		splash_hash: null,
		splash_width: null,
		splash_height: null,
		splash_card_alignment: null,
		embed_splash_hash: null,
		embed_splash_width: null,
		embed_splash_height: null,
		features: new Set(features),
		verification_level: 0,
		mfa_level: 0,
		nsfw_level: 0,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		system_channel_id: null,
		system_channel_flags: 0,
		rules_channel_id: null,
		afk_channel_id: null,
		afk_timeout: 0,
		disabled_operations: 0,
		member_count: 12,
		audit_logs_indexed_at: null,
		members_indexed_at: null,
		message_history_cutoff: null,
		invites_paused_until: pausedUntil,
		version: 1,
	};
}

class FakeGuilds {
	row: GuildRow | null = null;
	writes = 0;

	current(): Guild {
		return new Guild(this.row!);
	}

	async findUnique(_guildId: GuildID): Promise<Guild | null> {
		return this.row ? new Guild(this.row) : null;
	}

	async upsertPartial(_guildId: GuildID, patch: Partial<GuildRow>): Promise<Guild> {
		this.writes++;
		this.row = {...this.row!, ...patch, version: this.row!.version + 1};
		return new Guild(this.row);
	}
}

class CapturingPublisher implements ActivityPublisher {
	readonly events: Array<Event> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.events.push(JSON.parse(payload) as Event);
	}

	featureChanges() {
		return this.events.flatMap((event) => (event.kind === 'guild_features_changed' ? [event] : []));
	}
}

interface Harness {
	guilds: FakeGuilds;
	schedule: MockKVProvider;
	dispatched: Array<unknown>;
	audits: Array<{userId: bigint; action: number; changes: unknown}>;
	clock: {now: number};
	deps: GuildInvitePauseDeps;
}

function harness(): Harness {
	const guilds = new FakeGuilds();
	const schedule = new MockKVProvider();
	const h: Harness = {guilds, schedule, dispatched: [], audits: [], clock: {now: NOW}, deps: null as never};
	const auditService = new GuildAuditLogService(null as never, null as never, null as never, null as never);
	h.deps = {
		guilds,
		locks: {acquireLock: async () => 'token', releaseLock: async () => true},
		schedule,
		gateway: {
			dispatchGuild: async (params) => {
				h.dispatched.push(params.data);
			},
		},
		auditLog: {
			computeChanges: (previous, next) => auditService.computeChanges(previous, next),
			record: async (params) => {
				h.audits.push({userId: params.userId, action: params.action, changes: params.changes});
			},
		},
		now: () => h.clock.now,
	};
	return h;
}

async function scheduled(h: Harness): Promise<Array<string>> {
	return h.schedule.zrangebyscore(GUILD_INVITE_PAUSE_SCHEDULE_KEY, '-inf', '+inf');
}

describe('guild invite pause', () => {
	let h: Harness;
	let publisher: CapturingPublisher;

	beforeEach(async () => {
		h = harness();
		publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
	});

	afterEach(() => {
		resetActivityEventsForTests();
	});

	it('leaves a pause the guild set itself alone and reports it as already paused', async () => {
		h.guilds.row = guildRow([...BASE_FEATURES, GuildFeatures.INVITES_DISABLED]);
		const outcome = await applyPauseGuildInvites(h.deps, pauseEnvelope());
		expect(outcome).toEqual({
			action_id: 'a:11:guild:0',
			action_type: 'pause_guild_invites',
			status: 'noop',
			detail: 'already_paused',
			observed: null,
			guild_id: GUILD_ID,
		});
		expect(h.guilds.writes).toBe(0);
		expect(await scheduled(h)).toEqual([]);
		expect(publisher.events).toEqual([]);
		const lifted = await applyPauseGuildInvites(h.deps, pauseEnvelope({on: false}));
		expect(lifted.status).toBe('noop');
		expect(h.guilds.current().features.has(GuildFeatures.INVITES_DISABLED)).toBe(true);
	});

	it('pauses invites, records it as the system user and schedules the lift', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		const outcome = await applyPauseGuildInvites(h.deps, pauseEnvelope());
		expect(outcome).toMatchObject({status: 'applied', detail: `until_ms=${NOW + 2 * HOUR_MS}`, guild_id: GUILD_ID});
		const guild = h.guilds.current();
		expect([...guild.features].sort()).toEqual(
			[...BASE_FEATURES, GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED].sort(),
		);
		expect(guild.invitesPausedUntil?.getTime()).toBe(NOW + 2 * HOUR_MS);
		expect(h.dispatched).toHaveLength(1);
		expect(h.audits).toEqual([
			{
				userId: SYSTEM_USER_ID,
				action: AuditLogActionType.GUILD_UPDATE,
				changes: [
					{
						key: 'features',
						old_value: [...BASE_FEATURES],
						new_value: [...BASE_FEATURES, GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED].sort(),
					},
				],
			},
		]);
		expect(await scheduled(h)).toEqual([GUILD_ID]);
		const [event] = publisher.featureChanges();
		expect(event?.key).toBe(`guild:${GUILD_ID}`);
		expect(event?.meta.channel).toBe('worker');
		expect(event?.data).toEqual({
			guild_id: GUILD_ID,
			actor_id: null,
			source: 'action',
			added: [GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED],
			removed: [],
			invites_paused_until_ms: NOW + 2 * HOUR_MS,
		});
	});

	it('extends its own pause but never shortens it', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		await applyPauseGuildInvites(h.deps, pauseEnvelope());
		const extended = await applyPauseGuildInvites(
			h.deps,
			pauseEnvelope({id: 'a:11:guild:1', until_ms: NOW + 3 * HOUR_MS}),
		);
		expect(extended.status).toBe('applied');
		expect(h.guilds.current().invitesPausedUntil?.getTime()).toBe(NOW + 3 * HOUR_MS);
		expect(
			await h.schedule.zrangebyscore(GUILD_INVITE_PAUSE_SCHEDULE_KEY, NOW + 3 * HOUR_MS, NOW + 3 * HOUR_MS),
		).toEqual([GUILD_ID]);
		const shorter = await applyPauseGuildInvites(h.deps, pauseEnvelope({id: 'a:11:guild:2', until_ms: NOW + HOUR_MS}));
		expect(shorter).toMatchObject({status: 'noop', detail: `until_ms=${NOW + 3 * HOUR_MS}`});
		expect(h.guilds.current().invitesPausedUntil?.getTime()).toBe(NOW + 3 * HOUR_MS);
		expect(publisher.featureChanges().map((event) => event.data.invites_paused_until_ms)).toEqual([
			NOW + 2 * HOUR_MS,
			NOW + 3 * HOUR_MS,
		]);
	});

	it('lifts only its own pause and removes both features', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		await applyPauseGuildInvites(h.deps, pauseEnvelope());
		const outcome = await applyAction(
			{js: {} as never, state: {} as never, guildInvites: h.deps, now: () => NOW},
			pauseEnvelope({id: 'a:11:guild:3', on: false}),
		);
		expect(outcome).toMatchObject({status: 'applied', guild_id: GUILD_ID});
		const guild = h.guilds.current();
		expect([...guild.features]).toEqual(BASE_FEATURES);
		expect(guild.invitesPausedUntil).toBeNull();
		expect(await scheduled(h)).toEqual([]);
		expect(publisher.featureChanges().at(-1)?.data).toMatchObject({
			source: 'action',
			removed: [GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED],
			invites_paused_until_ms: null,
		});
		const again = await applyPauseGuildInvites(h.deps, pauseEnvelope({id: 'a:11:guild:4', on: false}));
		expect(again.status).toBe('noop');
	});

	it('ignores a late lift for an earlier window once the pause was extended', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		await applyPauseGuildInvites(h.deps, pauseEnvelope());
		await applyPauseGuildInvites(h.deps, pauseEnvelope({id: 'a:11:guild:5', until_ms: NOW + 3 * HOUR_MS}));
		const late = await applyPauseGuildInvites(
			h.deps,
			pauseEnvelope({id: 'a:11:guild:6', on: false, until_ms: NOW + 2 * HOUR_MS}),
		);
		expect(late).toMatchObject({status: 'noop', detail: `until_ms=${NOW + 3 * HOUR_MS}`});
		expect(h.guilds.current().features.has(GuildFeatures.INVITES_DISABLED)).toBe(true);
		expect(h.guilds.current().invitesPausedUntil?.getTime()).toBe(NOW + 3 * HOUR_MS);
		const current = await applyPauseGuildInvites(
			h.deps,
			pauseEnvelope({id: 'a:11:guild:7', on: false, until_ms: NOW + 3 * HOUR_MS}),
		);
		expect(current.status).toBe('applied');
		expect(h.guilds.current().invitesPausedUntil).toBeNull();
	});

	it('keeps the lift scheduled when the gateway fails after the write and a redelivery changes nothing', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		const dispatch = h.deps.gateway.dispatchGuild;
		let failures = 1;
		h.deps.gateway = {
			dispatchGuild: async (params) => {
				if (failures-- > 0) throw new Error('gateway timeout');
				await dispatch(params);
			},
		};
		const first = await applyPauseGuildInvites(h.deps, pauseEnvelope());
		expect(first).toMatchObject({status: 'applied', guild_id: GUILD_ID});
		expect(h.audits).toHaveLength(1);
		expect(publisher.featureChanges()).toHaveLength(1);
		const redelivered = await applyPauseGuildInvites(h.deps, pauseEnvelope());
		expect(redelivered).toMatchObject({status: 'noop', detail: `until_ms=${NOW + 2 * HOUR_MS}`});
		expect(h.guilds.writes).toBe(1);
		expect(await scheduled(h)).toEqual([GUILD_ID]);
		h.clock.now = NOW + 2 * HOUR_MS;
		expect(await liftExpiredGuildInvitePauses(h.deps, 100)).toBe(1);
		expect([...h.guilds.current().features]).toEqual(BASE_FEATURES);
	});

	it('restores a lost schedule entry when the same pause is redelivered', async () => {
		h.guilds.row = guildRow(
			[...BASE_FEATURES, GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED],
			new Date(NOW + 2 * HOUR_MS),
		);
		const outcome = await applyPauseGuildInvites(h.deps, pauseEnvelope());
		expect(outcome.status).toBe('noop');
		expect(h.guilds.writes).toBe(0);
		expect(await scheduled(h)).toEqual([GUILD_ID]);
	});

	it('writes nothing when scheduling fails so the retry applies the pause', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		const zadd = h.schedule.zadd.bind(h.schedule);
		let failures = 1;
		h.deps.schedule = {
			zadd: async (...args: Parameters<typeof zadd>) => {
				if (failures-- > 0) throw new Error('kv unavailable');
				return zadd(...args);
			},
			zrem: h.schedule.zrem.bind(h.schedule),
			zrangebyscore: h.schedule.zrangebyscore.bind(h.schedule),
		};
		await expect(applyPauseGuildInvites(h.deps, pauseEnvelope())).rejects.toThrow('kv unavailable');
		expect(h.guilds.writes).toBe(0);
		expect((await applyPauseGuildInvites(h.deps, pauseEnvelope())).status).toBe('applied');
		expect(await scheduled(h)).toEqual([GUILD_ID]);
	});

	it('routes the action through the consumer and answers every outcome with the guild', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		const consumer = {js: {} as never, state: {} as never, guildInvites: h.deps, now: () => NOW};
		expect(await applyAction(consumer, pauseEnvelope())).toMatchObject({status: 'applied', guild_id: GUILD_ID});
		expect(await applyAction(consumer, pauseEnvelope({expires_at_ms: NOW}))).toMatchObject({
			status: 'expired',
			guild_id: GUILD_ID,
		});
		expect(await applyAction(consumer, {...pauseEnvelope(), v: 2})).toMatchObject({
			status: 'unsupported',
			guild_id: GUILD_ID,
		});
		h.guilds.row = guildRow([...BASE_FEATURES, GuildFeatures.INVITES_DISABLED]);
		expect(await applyAction(consumer, pauseEnvelope({id: 'a:11:guild:5'}))).toMatchObject({
			status: 'noop',
			detail: 'already_paused',
			guild_id: GUILD_ID,
		});
		expect(h.guilds.writes).toBe(1);
	});

	it('removes a notice staff set before the pause when the pause ends', async () => {
		h.guilds.row = guildRow([...BASE_FEATURES, GuildFeatures.RAID_DETECTED]);
		expect((await applyPauseGuildInvites(h.deps, pauseEnvelope())).status).toBe('applied');
		h.clock.now = NOW + 2 * HOUR_MS;
		expect(await liftExpiredGuildInvitePauses(h.deps, 100)).toBe(1);
		expect([...h.guilds.current().features]).toEqual(BASE_FEATURES);
	});

	it('reports a missing guild as ineligible', async () => {
		expect((await applyPauseGuildInvites(h.deps, pauseEnvelope())).status).toBe('ineligible');
	});

	it('lifts an expired pause on the sweep without any action', async () => {
		h.guilds.row = guildRow(BASE_FEATURES);
		await applyPauseGuildInvites(h.deps, pauseEnvelope());
		expect(await liftExpiredGuildInvitePauses(h.deps, 100)).toBe(0);
		expect(h.guilds.current().features.has(GuildFeatures.INVITES_DISABLED)).toBe(true);
		h.clock.now = NOW + 2 * HOUR_MS;
		expect(await liftExpiredGuildInvitePauses(h.deps, 100)).toBe(1);
		const guild = h.guilds.current();
		expect([...guild.features]).toEqual(BASE_FEATURES);
		expect(guild.invitesPausedUntil).toBeNull();
		expect(await scheduled(h)).toEqual([]);
		expect(publisher.featureChanges().at(-1)?.data).toMatchObject({
			source: 'system',
			actor_id: null,
			removed: [GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED],
		});
	});

	it('drops a sweep entry once the pause is no longer ours and reschedules an extended one', async () => {
		h.guilds.row = guildRow([...BASE_FEATURES, GuildFeatures.INVITES_DISABLED]);
		await h.schedule.zadd(GUILD_INVITE_PAUSE_SCHEDULE_KEY, NOW - 1, GUILD_ID);
		expect(await liftExpiredGuildInvitePauses(h.deps, 100)).toBe(0);
		expect(await scheduled(h)).toEqual([]);
		expect(h.guilds.current().features.has(GuildFeatures.INVITES_DISABLED)).toBe(true);
		h.guilds.row = guildRow(
			[...BASE_FEATURES, GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED],
			new Date(NOW + HOUR_MS),
		);
		await h.schedule.zadd(GUILD_INVITE_PAUSE_SCHEDULE_KEY, NOW - 1, GUILD_ID);
		expect(await liftExpiredGuildInvitePauses(h.deps, 100)).toBe(0);
		expect(await h.schedule.zrangebyscore(GUILD_INVITE_PAUSE_SCHEDULE_KEY, NOW + HOUR_MS, NOW + HOUR_MS)).toEqual([
			GUILD_ID,
		]);
		expect(h.guilds.writes).toBe(0);
	});
});
