// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Event} from '@app/api/infrastructure/activity/Contract.generated';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

class CapturingPublisher implements ActivityPublisher {
	readonly events: Array<Event> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.events.push(JSON.parse(payload) as Event);
	}

	of<K extends Event['kind']>(kind: K): Array<Extract<Event, {kind: K}>> {
		return this.events.filter((event): event is Extract<Event, {kind: K}> => event.kind === kind);
	}
}

describe('moderation activity events', () => {
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

	async function ban(token: string, path: string): Promise<void> {
		await createBuilder(harness, token).put(path).body({}).expect(HTTP_STATUS.NO_CONTENT).execute();
	}

	test('a moderator ban and unban publish facts about the target without addresses', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		await ban(owner.token, `/guilds/${guild.id}/bans/${target.userId}`);
		const [banned] = publisher.of('guild_member_banned');
		expect(banned?.key).toBe(target.userId);
		expect(banned?.data).toEqual({
			guild_id: guild.id,
			user_id: target.userId,
			moderator_id: owner.userId,
			by: 'moderator',
			guild_member_count: 2,
			target_moderator: false,
			expires_at_ms: null,
		});
		expect(banned?.meta).toMatchObject({ip: null, country: null, ua: null, locale: null});
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const [unbanned] = publisher.of('guild_member_unbanned');
		expect(unbanned?.data).toEqual({
			guild_id: guild.id,
			user_id: target.userId,
			moderator_id: owner.userId,
			by: 'moderator',
		});
		expect(unbanned?.meta.ip).toBeNull();
	});
});
