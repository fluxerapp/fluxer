// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {
	applyPauseGuildInvites,
	endExpiredPlannedJoinWaves,
	GUILD_PLANNED_JOIN_WAVES_KEY,
	GUILD_PLANNED_JOIN_WAVES_SCHEDULE_KEY,
	type GuildInvitePauseDeps,
	restatePlannedJoinWaves,
} from '@app/api/guild/services/GuildInvitePauseApplier';
import {acceptInvite, createChannelInvite, createGuild, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Event} from '@app/api/infrastructure/activity/Contract.generated';
import {getGatewayService, getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getCacheService, getGuildAuditLogService, getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const HOUR_MS = 3_600_000;

class CapturingPublisher implements ActivityPublisher {
	readonly events: Array<Event> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.events.push(JSON.parse(payload) as Event);
	}

	of<K extends Event['kind']>(kind: K): Array<Extract<Event, {kind: K}>> {
		return this.events.filter((event): event is Extract<Event, {kind: K}> => event.kind === kind);
	}
}

function pauseDeps(): GuildInvitePauseDeps {
	return {
		guilds: getGuildRepository(),
		locks: getCacheService(),
		schedule: getKVClient(),
		gateway: getGatewayService(),
		auditLog: getGuildAuditLogService(),
	};
}

async function pauseInvites(guildId: string, untilMs: number): Promise<void> {
	const outcome = await applyPauseGuildInvites(pauseDeps(), {
		v: 1,
		id: `a:11:${guildId}:0`,
		key: `guild:${guildId}`,
		issued_at_ms: Date.now(),
		expires_at_ms: Date.now() + 60_000,
		type: 'pause_guild_invites',
		guild_id: guildId,
		on: true,
		until_ms: untilMs,
	});
	expect(outcome.status).toBe('applied');
}

async function storedPause(guildId: string): Promise<{features: Set<string>; until: Date | null}> {
	const guild = await getGuildRepository().findUnique(createGuildID(BigInt(guildId)));
	return {features: new Set(guild!.features), until: guild!.invitesPausedUntil};
}

describe('guild activity events', () => {
	let harness: ApiTestHarness;
	let publisher: CapturingPublisher;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
	});

	afterEach(async () => {
		resetActivityEventsForTests();
		await harness?.shutdown();
	});

	describe('managed invite pause', () => {
		let owner: Awaited<ReturnType<typeof createTestAccount>>;
		let guild: GuildResponse;
		let untilMs: number;

		beforeEach(async () => {
			owner = await createTestAccount(harness);
			guild = await createGuild(harness, owner.token, 'Managed pause');
			untilMs = Date.now() + 2 * HOUR_MS;
			await pauseInvites(guild.id, untilMs);
		});

		test('the owner turning invites back on ends the pause and drops the notice', async () => {
			const paused = await storedPause(guild.id);
			expect(paused.until?.getTime()).toBe(untilMs);
			expect(paused.features.has(GuildFeatures.RAID_DETECTED)).toBe(true);
			const features = [...paused.features].filter((feature) => feature !== GuildFeatures.INVITES_DISABLED);
			const updated = await updateGuild(harness, owner.token, guild.id, {features});
			expect(updated.features).not.toContain(GuildFeatures.INVITES_DISABLED);
			expect(updated.features).not.toContain(GuildFeatures.RAID_DETECTED);
			const after = await storedPause(guild.id);
			expect(after.until).toBeNull();
			const memberChange = publisher.of('guild_features_changed').at(-1);
			expect(memberChange?.data).toEqual({
				guild_id: guild.id,
				actor_id: owner.userId,
				source: 'member',
				added: [],
				removed: [GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED],
				invites_paused_until_ms: null,
			});
			const joiner = await createTestAccount(harness);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, joiner.token, invite.code);
		});

		test('an unrelated member edit keeps the pause', async () => {
			await updateGuild(harness, owner.token, guild.id, {name: 'Renamed'});
			expect((await storedPause(guild.id)).until?.getTime()).toBe(untilMs);
		});

		test('a staff edit of the pause features takes the pause over from the timer', async () => {
			const admin = await setUserACLs(harness, await createTestAccount(harness), [
				'admin:authenticate',
				'guild:update:features',
			]);
			await createBuilder(harness, admin.token)
				.patch(`/admin/guilds/${guild.id}`)
				.body({remove_features: [GuildFeatures.RAID_DETECTED]})
				.expect(HTTP_STATUS.OK)
				.execute();
			const after = await storedPause(guild.id);
			expect(after.until).toBeNull();
			expect(after.features.has(GuildFeatures.INVITES_DISABLED)).toBe(true);
			expect(after.features.has(GuildFeatures.RAID_DETECTED)).toBe(false);
			expect(publisher.of('guild_features_changed').at(-1)?.data).toEqual({
				guild_id: guild.id,
				actor_id: admin.userId,
				source: 'staff',
				added: [],
				removed: [GuildFeatures.RAID_DETECTED],
				invites_paused_until_ms: null,
			});
		});

		test('a staff edit of other features leaves the timer running', async () => {
			const admin = await setUserACLs(harness, await createTestAccount(harness), [
				'admin:authenticate',
				'guild:update:features',
			]);
			await createBuilder(harness, admin.token)
				.patch(`/admin/guilds/${guild.id}`)
				.body({add_features: [GuildFeatures.VERIFIED]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await storedPause(guild.id)).until?.getTime()).toBe(untilMs);
		});
	});

	describe('planned join waves end date', () => {
		let guild: GuildResponse;
		let admin: Awaited<ReturnType<typeof createTestAccount>>;

		beforeEach(async () => {
			const owner = await createTestAccount(harness);
			guild = await createGuild(harness, owner.token, 'Planned waves');
			admin = await setUserACLs(harness, await createTestAccount(harness), [
				'admin:authenticate',
				'guild:update:features',
			]);
		});

		async function staffEdit(body: Record<string, unknown>, status: number = HTTP_STATUS.OK): Promise<void> {
			await createBuilder(harness, admin.token).patch(`/admin/guilds/${guild.id}`).body(body).expect(status).execute();
		}

		async function stored(): Promise<{features: Set<string>; until: Date | null}> {
			const row = await getGuildRepository().findUnique(createGuildID(BigInt(guild.id)));
			return {features: new Set(row!.features), until: row!.plannedJoinWavesUntil};
		}

		async function scheduledAt(): Promise<number | null> {
			const kv = getKVClient();
			const all = await kv.zrangebyscore(GUILD_PLANNED_JOIN_WAVES_SCHEDULE_KEY, '-inf', '+inf');
			if (!all.includes(guild.id)) return null;
			const stored = (await getGuildRepository().findUnique(createGuildID(BigInt(guild.id))))?.plannedJoinWavesUntil;
			const at = stored?.getTime() ?? -1;
			return (await kv.zrangebyscore(GUILD_PLANNED_JOIN_WAVES_SCHEDULE_KEY, at, at)).includes(guild.id) ? at : -1;
		}

		async function tracked(): Promise<boolean> {
			return (await getKVClient().smembers(GUILD_PLANNED_JOIN_WAVES_KEY)).includes(guild.id);
		}

		async function storeEnd(at: Date): Promise<void> {
			const repository = getGuildRepository();
			const current = await repository.findUnique(createGuildID(BigInt(guild.id)));
			await repository.upsertPartial(current!.id, {planned_join_waves_until: at}, current!.toRow());
		}

		test('staff edits keep the tracking set in step and restore a missing entry', async () => {
			await staffEdit({add_features: [GuildFeatures.PLANNED_JOIN_WAVES]});
			expect(await tracked()).toBe(true);
			await getKVClient().srem(GUILD_PLANNED_JOIN_WAVES_KEY, guild.id);
			await staffEdit({add_features: [GuildFeatures.VERIFIED]});
			expect(await tracked()).toBe(true);
			await staffEdit({remove_features: [GuildFeatures.PLANNED_JOIN_WAVES]});
			expect(await tracked()).toBe(false);
		});

		test('the daily restatement tells downstream consumers a permanent exemption is still on', async () => {
			await staffEdit({add_features: [GuildFeatures.PLANNED_JOIN_WAVES]});
			const staffAdd = publisher.of('guild_features_changed').at(-1);
			expect(await restatePlannedJoinWaves(pauseDeps())).toBeGreaterThanOrEqual(1);
			const restated = publisher
				.of('guild_features_changed')
				.filter((event) => event.data.guild_id === guild.id)
				.at(-1);
			expect(restated?.id).not.toBe(staffAdd?.id);
			expect(restated?.data).toEqual({
				guild_id: guild.id,
				actor_id: null,
				source: 'staff',
				added: [GuildFeatures.PLANNED_JOIN_WAVES],
				removed: [],
				invites_paused_until_ms: null,
			});
		});

		test('resaving an end date that has just passed changes nothing', async () => {
			await staffEdit({
				add_features: [GuildFeatures.PLANNED_JOIN_WAVES],
				planned_join_waves_until: new Date(Date.now() + HOUR_MS).toISOString(),
			});
			const passed = new Date(Math.floor((Date.now() - 60_000) / 60_000) * 60_000 + 30_000);
			await storeEnd(passed);
			const asTheFormSendsIt = new Date(Math.floor(passed.getTime() / 60_000) * 60_000).toISOString();
			await staffEdit({planned_join_waves_until: asTheFormSendsIt});
			const after = await stored();
			expect(after.features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(true);
			expect(after.until?.getTime()).toBe(passed.getTime());
			await staffEdit(
				{planned_join_waves_until: new Date(passed.getTime() - 3_600_000).toISOString()},
				HTTP_STATUS.BAD_REQUEST,
			);
			expect((await stored()).until?.getTime()).toBe(passed.getTime());
		});

		test('adding it without an end date keeps it on with nothing scheduled', async () => {
			await staffEdit({add_features: [GuildFeatures.PLANNED_JOIN_WAVES]});
			const after = await stored();
			expect(after.features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(true);
			expect(after.until).toBeNull();
			expect(await scheduledAt()).toBeNull();
			expect(await endExpiredPlannedJoinWaves(pauseDeps(), 100)).toBe(0);
			expect((await stored()).features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(true);
		});

		test('an end date is stored, scheduled, moved by a new date and cleared by a permanent one', async () => {
			const first = Date.now() + 2 * HOUR_MS;
			await staffEdit({
				add_features: [GuildFeatures.PLANNED_JOIN_WAVES],
				planned_join_waves_until: new Date(first).toISOString(),
			});
			expect((await stored()).until?.getTime()).toBe(first);
			expect(await scheduledAt()).toBe(first);
			const second = Date.now() + 5 * HOUR_MS;
			await staffEdit({planned_join_waves_until: new Date(second).toISOString()});
			expect((await stored()).until?.getTime()).toBe(second);
			expect(await scheduledAt()).toBe(second);
			await staffEdit({planned_join_waves_until: null});
			const after = await stored();
			expect(after.features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(true);
			expect(after.until).toBeNull();
			expect(await scheduledAt()).toBeNull();
		});

		test('removing it clears the end date and the schedule', async () => {
			await staffEdit({
				add_features: [GuildFeatures.PLANNED_JOIN_WAVES],
				planned_join_waves_until: new Date(Date.now() + 2 * HOUR_MS).toISOString(),
			});
			await staffEdit({remove_features: [GuildFeatures.PLANNED_JOIN_WAVES]});
			const after = await stored();
			expect(after.features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(false);
			expect(after.until).toBeNull();
			expect(await scheduledAt()).toBeNull();
		});

		test('an end date in the past or without the feature is refused', async () => {
			await staffEdit(
				{
					add_features: [GuildFeatures.PLANNED_JOIN_WAVES],
					planned_join_waves_until: new Date(Date.now() - 1000).toISOString(),
				},
				HTTP_STATUS.BAD_REQUEST,
			);
			await staffEdit(
				{planned_join_waves_until: new Date(Date.now() + HOUR_MS).toISOString()},
				HTTP_STATUS.BAD_REQUEST,
			);
			const after = await stored();
			expect(after.features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(false);
			expect(after.until).toBeNull();
			expect(await scheduledAt()).toBeNull();
		});

		test('the sweep removes it after the end date and tells downstream consumers', async () => {
			const until = Date.now() + 2 * HOUR_MS;
			await staffEdit({
				add_features: [GuildFeatures.PLANNED_JOIN_WAVES],
				planned_join_waves_until: new Date(until).toISOString(),
			});
			expect(publisher.of('guild_features_changed').at(-1)?.data).toMatchObject({
				source: 'staff',
				added: [GuildFeatures.PLANNED_JOIN_WAVES],
			});
			expect(await endExpiredPlannedJoinWaves({...pauseDeps(), now: () => until - 1}, 100)).toBe(0);
			expect((await stored()).features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(true);
			expect(await endExpiredPlannedJoinWaves({...pauseDeps(), now: () => until}, 100)).toBe(1);
			const after = await stored();
			expect(after.features.has(GuildFeatures.PLANNED_JOIN_WAVES)).toBe(false);
			expect(after.until).toBeNull();
			expect(await scheduledAt()).toBeNull();
			expect(await tracked()).toBe(false);
			expect(publisher.of('guild_features_changed').at(-1)?.data).toEqual({
				guild_id: guild.id,
				actor_id: null,
				source: 'system',
				added: [],
				removed: [GuildFeatures.PLANNED_JOIN_WAVES],
				invites_paused_until_ms: null,
			});
		});
	});
});
