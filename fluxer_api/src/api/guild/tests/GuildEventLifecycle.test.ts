// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {GuildEventRepository} from '@app/api/guild/repositories/GuildEventRepository';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {setupTestGuildWithMembers, updateRole} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildEvent, GuildEventCreate} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

function eventData(overrides: Partial<GuildEventCreate> = {}): GuildEventCreate {
	return {
		name: 'Community game night',
		description: 'Bring your favourite game',
		location: 'Gaming voice channel',
		starts_at: '2030-06-10T18:00:00.000Z',
		ends_at: '2030-06-10T20:00:00.000Z',
		...overrides,
	};
}

async function createEvent(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	body: GuildEventCreate = eventData(),
): Promise<GuildEvent> {
	return createBuilder<GuildEvent>(harness, token)
		.post(`/guilds/${guildId}/events`)
		.body(body)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function listEvents(harness: ApiTestHarness, token: string, guildId: string): Promise<Array<GuildEvent>> {
	return createBuilder<Array<GuildEvent>>(harness, token)
		.get(`/guilds/${guildId}/events`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Guild event lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('creates, lists, updates, and deletes a community event', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const created = await createEvent(harness, owner.token, guild.id);
		expect(created).toMatchObject({
			guild_id: guild.id,
			creator_id: owner.userId,
			name: 'Community game night',
			description: 'Bring your favourite game',
			location: 'Gaming voice channel',
			starts_at: '2030-06-10T18:00:00.000Z',
			ends_at: '2030-06-10T20:00:00.000Z',
			image_url: null,
		});
		expect(created.id).toMatch(/^\d+$/);
		expect(created.created_at).toMatch(/^\d{4}-\d{2}-\d{2}T/);

		const listed = await listEvents(harness, owner.token, guild.id);
		expect(listed).toEqual([created]);

		const updated = await createBuilder<GuildEvent>(harness, owner.token)
			.patch(`/guilds/${guild.id}/events/${created.id}`)
			.body({
				name: 'Late game night',
				location: null,
				ends_at: '2030-06-10T21:00:00.000Z',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated).toMatchObject({
			id: created.id,
			creator_id: owner.userId,
			name: 'Late game night',
			description: created.description,
			location: null,
			starts_at: created.starts_at,
			ends_at: '2030-06-10T21:00:00.000Z',
		});
		expect(await listEvents(harness, owner.token, guild.id)).toEqual([updated]);

		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/events/${created.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await listEvents(harness, owner.token, guild.id)).toEqual([]);
	});

	test('lists events for members without create or manage permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: (DEFAULT_PERMISSIONS & ~Permissions.CREATE_EVENTS & ~Permissions.MANAGE_EVENTS).toString(),
		});
		const event = await createEvent(harness, owner.token, guild.id);
		expect(await listEvents(harness, member.token, guild.id)).toEqual([event]);
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/events`)
			.body(eventData({name: 'Denied'}))
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('lets a creator edit and delete their event without MANAGE_EVENTS', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const creator = members[0]!;
		const other = members[1]!;
		const event = await createEvent(harness, creator.token, guild.id, eventData({name: 'Creator session'}));
		const updated = await createBuilder<GuildEvent>(harness, creator.token)
			.patch(`/guilds/${guild.id}/events/${event.id}`)
			.body({description: 'Updated by creator'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.description).toBe('Updated by creator');
		await createBuilder(harness, other.token)
			.patch(`/guilds/${guild.id}/events/${event.id}`)
			.body({name: 'Taken over'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, creator.token)
			.delete(`/guilds/${guild.id}/events/${event.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await listEvents(harness, owner.token, guild.id)).toEqual([]);
	});

	test('orders listed events by start time', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const later = await createEvent(
			harness,
			owner.token,
			guild.id,
			eventData({name: 'Later', starts_at: '2030-06-12T18:00:00.000Z', ends_at: '2030-06-12T19:00:00.000Z'}),
		);
		const earlier = await createEvent(
			harness,
			owner.token,
			guild.id,
			eventData({name: 'Earlier', starts_at: '2030-06-11T18:00:00.000Z', ends_at: '2030-06-11T19:00:00.000Z'}),
		);
		expect((await listEvents(harness, owner.token, guild.id)).map((event) => event.id)).toEqual([earlier.id, later.id]);
	});

	test('purges guild_events rows when the guild record is deleted', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const event = await createEvent(harness, owner.token, guild.id);
		const guildId = createGuildID(BigInt(guild.id));
		const events = new GuildEventRepository();
		expect((await events.list(guildId)).map((row) => row.id.toString())).toEqual([event.id]);
		await new GuildRepository().delete(guildId);
		expect(await events.list(guildId)).toEqual([]);
	});
});
