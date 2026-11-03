// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {applyPauseGuildInvites, type GuildInvitePauseDeps} from '@app/api/guild/services/GuildInvitePauseApplier';
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
});
