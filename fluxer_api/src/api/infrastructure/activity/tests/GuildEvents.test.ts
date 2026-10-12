// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {applyPauseGuildInvites, type GuildInvitePauseDeps} from '@app/api/guild/services/GuildInvitePauseApplier';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	deleteInvite,
	leaveGuild,
	setupTestGuildWithMembers,
	updateGuild,
} from '@app/api/guild/tests/GuildTestUtils';
import {
	idleActivityEvents,
	resetActivityEventsForTests,
	startActivityEvents,
} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Event} from '@app/api/infrastructure/activity/Contract.generated';
import {getGatewayService, getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getCacheService, getGuildAuditLogService, getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
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

	test('creating a guild publishes who owns it', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Fresh guild');
		const [created] = publisher.of('guild_created');
		expect(created?.key).toBe(owner.userId);
		expect(created?.data).toEqual({guild_id: guild.id, owner_id: owner.userId, member_count: 1});
	});

	test('a member feature toggle publishes the actor and the member source', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Toggle guild');
		await updateGuild(harness, owner.token, guild.id, {features: [...guild.features, GuildFeatures.INVITES_DISABLED]});
		const [changed] = publisher.of('guild_features_changed');
		expect(changed?.key).toBe(`guild:${guild.id}`);
		expect(changed?.data).toEqual({
			guild_id: guild.id,
			actor_id: owner.userId,
			source: 'member',
			added: [GuildFeatures.INVITES_DISABLED],
			removed: [],
			invites_paused_until_ms: null,
		});
	});

	test('creating and deleting an invite publishes both with the invite settings', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Invite guild');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await deleteInvite(harness, owner.token, invite.code);
		const [created] = publisher.of('invite_created');
		expect(created?.key).toBe(`guild:${guild.id}`);
		expect(created?.data).toEqual({
			guild_id: guild.id,
			channel_id: guild.system_channel_id,
			code: invite.code,
			inviter_id: owner.userId,
			max_uses: invite.max_uses,
			max_age_s: invite.max_age,
			temporary: false,
		});
		const [deleted] = publisher.of('invite_deleted');
		expect(deleted?.key).toBe(`guild:${guild.id}`);
		expect(deleted?.data).toEqual({guild_id: guild.id, code: invite.code});
	});

	test('leaving and being kicked publish the removal kind and actor', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [leaver, kicked] = members;
		await leaveGuild(harness, leaver!.token, guild.id);
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/members/${kicked!.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await idleActivityEvents();
		expect(publisher.of('guild_member_removed').map((event) => [event.key, event.data])).toEqual([
			[leaver!.userId, {guild_id: guild.id, user_id: leaver!.userId, actor_id: null, kind: 'left'}],
			[kicked!.userId, {guild_id: guild.id, user_id: kicked!.userId, actor_id: owner.userId, kind: 'kicked'}],
		]);
	});

	test('a join refused while invites are paused publishes the refusal only for non-members', async () => {
		const owner = await createTestAccount(harness);
		const joiner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Paused guild');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await pauseInvites(guild.id, Date.now() + HOUR_MS);
		for (const account of [owner, joiner]) {
			await createBuilder(harness, account.token)
				.post(`/invites/${invite.code}`)
				.body(null)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVITES_DISABLED)
				.execute();
		}
		await idleActivityEvents();
		const refusals = publisher.of('guild_join_refused');
		expect(refusals).toHaveLength(1);
		const [refused] = refusals;
		expect(refused?.key).toBe(joiner.userId);
		expect(refused?.data).toEqual({
			guild_id: guild.id,
			user_id: joiner.userId,
			cause: 'invites_paused',
			join_source: 'invite',
		});
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
});
