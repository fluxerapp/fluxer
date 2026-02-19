/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {GuildRecord} from '@app/records/GuildRecord';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import MemberSidebarStore from '@app/stores/MemberSidebarStore';
import {buildMemberListLayout} from '@app/utils/MemberListLayout';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

const DEFAULT_JOINED_AT = '2026-02-01T00:00:00.000Z';

function createUser(userId: string, username: string): UserPartialResponse {
	return {
		id: userId,
		username,
		discriminator: '0001',
		global_name: username,
		avatar: null,
		avatar_color: null,
		flags: 0,
	};
}

function createMember(_guildId: string, userId: string, username: string): GuildMemberData {
	return {
		user: createUser(userId, username),
		nick: null,
		avatar: null,
		banner: null,
		accent_color: null,
		roles: [],
		joined_at: DEFAULT_JOINED_AT,
		mute: false,
		deaf: false,
		communication_disabled_until: null,
		profile_flags: 0,
	};
}

function seedMembers(guildId: string, members: Array<{id: string; name: string}>): void {
	for (const member of members) {
		GuildMemberStore.handleMemberAdd(guildId, createMember(guildId, member.id, member.name));
	}
}

function createGuild(guildId: string, disabledOperations = 0): GuildRecord {
	const guild: Guild = {
		id: guildId,
		name: `Guild ${guildId}`,
		icon: null,
		vanity_url_code: null,
		owner_id: 'owner-1',
		system_channel_id: null,
		features: [],
		disabled_operations: disabledOperations,
	};
	return new GuildRecord(guild);
}

describe('MemberSidebarStore', () => {
	beforeEach(() => {
		MemberSidebarStore.handleSessionInvalidated();
		GuildMemberStore.handleConnectionOpen([]);
		GuildStore.guilds = {};
	});

	test('stores members by member index when sync includes group entries', () => {
		const guildId = 'guild-1';
		const listId = 'list-1';
		seedMembers(guildId, [
			{id: 'u-1', name: 'Alpha'},
			{id: 'u-2', name: 'Bravo'},
			{id: 'u-3', name: 'Charlie'},
		]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 2,
			groups: [
				{id: 'online', count: 2},
				{id: 'offline', count: 1},
			],
			ops: [
				{
					op: 'SYNC',
					range: [0, 4],
					items: [
						{group: {id: 'online', count: 2}},
						{member: {user: {id: 'u-1'}}},
						{member: {user: {id: 'u-2'}}},
						{group: {id: 'offline', count: 1}},
						{member: {user: {id: 'u-3'}}},
					],
				},
			],
		});

		const listState = MemberSidebarStore.getList(guildId, listId);
		expect(listState?.items.size).toBe(3);
		expect(listState?.items.get(0)?.data.user.id).toBe('u-1');
		expect(listState?.items.get(1)?.data.user.id).toBe('u-2');
		expect(listState?.items.get(2)?.data.user.id).toBe('u-3');
	});

	test('moves members with delete and insert without breaking group boundaries', () => {
		const guildId = 'guild-2';
		const listId = 'list-2';
		seedMembers(guildId, [
			{id: 'u-1', name: 'Alpha'},
			{id: 'u-2', name: 'Bravo'},
			{id: 'u-3', name: 'Charlie'},
		]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 2,
			groups: [
				{id: 'online', count: 2},
				{id: 'offline', count: 1},
			],
			ops: [
				{
					op: 'SYNC',
					range: [0, 4],
					items: [
						{group: {id: 'online', count: 2}},
						{member: {user: {id: 'u-2'}}},
						{member: {user: {id: 'u-1'}}},
						{group: {id: 'offline', count: 1}},
						{member: {user: {id: 'u-3'}}},
					],
				},
			],
		});

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 1,
			groups: [
				{id: 'online', count: 1},
				{id: 'offline', count: 2},
			],
			ops: [
				{op: 'DELETE', index: 2},
				{op: 'INSERT', index: 4, item: {member: {user: {id: 'u-1'}}}},
			],
		});

		const listState = MemberSidebarStore.getList(guildId, listId);
		expect(listState?.items.get(0)?.data.user.id).toBe('u-2');
		expect(listState?.items.get(1)?.data.user.id).toBe('u-3');
		expect(listState?.items.get(2)?.data.user.id).toBe('u-1');

		const layouts = buildMemberListLayout(listState?.groups ?? []);
		const offlineLayout = layouts.find((layout) => layout.id === 'offline');
		expect(offlineLayout?.memberStartIndex).toBe(1);
		expect(offlineLayout?.memberEndIndex).toBe(2);
	});

	test('does not duplicate members when applying row-based deletes and inserts', () => {
		const guildId = 'guild-2b';
		const listId = 'list-2b';
		seedMembers(guildId, [
			{id: 'u-1', name: 'Alpha'},
			{id: 'u-2', name: 'Bravo'},
			{id: 'u-3', name: 'Charlie'},
		]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 2,
			groups: [
				{id: 'online', count: 2},
				{id: 'offline', count: 1},
			],
			ops: [
				{
					op: 'SYNC',
					range: [0, 4],
					items: [
						{group: {id: 'online', count: 2}},
						{member: {user: {id: 'u-1'}}},
						{member: {user: {id: 'u-2'}}},
						{group: {id: 'offline', count: 1}},
						{member: {user: {id: 'u-3'}}},
					],
				},
			],
		});

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 1,
			groups: [
				{id: 'online', count: 1},
				{id: 'offline', count: 2},
			],
			ops: [
				{op: 'DELETE', index: 1},
				{op: 'INSERT', index: 4, item: {member: {user: {id: 'u-1'}}}},
			],
		});

		const listState = MemberSidebarStore.getList(guildId, listId);
		const orderedUserIds = Array.from(listState?.items.values() ?? []).map((item) => item.data.user.id);
		expect(new Set(orderedUserIds).size).toBe(orderedUserIds.length);
		expect(orderedUserIds).toEqual(['u-2', 'u-3', 'u-1']);
	});

	test('invalidates row ranges and keeps remaining members in order', () => {
		const guildId = 'guild-3';
		const listId = 'list-3';
		seedMembers(guildId, [
			{id: 'u-1', name: 'Alpha'},
			{id: 'u-2', name: 'Bravo'},
			{id: 'u-3', name: 'Charlie'},
		]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 3,
			groups: [{id: 'online', count: 3}],
			ops: [
				{
					op: 'SYNC',
					range: [0, 3],
					items: [
						{group: {id: 'online', count: 3}},
						{member: {user: {id: 'u-1'}}},
						{member: {user: {id: 'u-2'}}},
						{member: {user: {id: 'u-3'}}},
					],
				},
			],
		});

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 3,
			groups: [{id: 'online', count: 3}],
			ops: [{op: 'INVALIDATE', range: [2, 2]}],
		});

		const listState = MemberSidebarStore.getList(guildId, listId);
		expect(listState?.items.get(0)?.data.user.id).toBe('u-1');
		expect(listState?.items.get(1)).toBeUndefined();
		expect(listState?.items.get(2)?.data.user.id).toBe('u-3');
		expect(Array.from(listState?.items.values() ?? []).map((item) => item.data.user.id)).toEqual(['u-1', 'u-3']);
	});

	test('dedupes duplicate user rows and ignores rows outside group bounds', () => {
		const guildId = 'guild-4';
		const listId = 'list-4';
		seedMembers(guildId, [
			{id: 'u-1', name: 'Alpha'},
			{id: 'u-2', name: 'Bravo'},
			{id: 'u-3', name: 'Charlie'},
		]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 3,
			onlineCount: 2,
			groups: [
				{id: 'online', count: 2},
				{id: 'offline', count: 1},
			],
			ops: [
				{
					op: 'SYNC',
					range: [0, 5],
					items: [
						{group: {id: 'online', count: 2}},
						{member: {user: {id: 'u-1'}}},
						{member: {user: {id: 'u-2'}}},
						{group: {id: 'offline', count: 1}},
						{member: {user: {id: 'u-1'}}},
						{member: {user: {id: 'u-3'}}},
					],
				},
			],
		});

		const listState = MemberSidebarStore.getList(guildId, listId);
		const orderedUserIds = Array.from(listState?.items.values() ?? []).map((item) => item.data.user.id);
		expect(orderedUserIds).toEqual(['u-1', 'u-2']);
		expect(listState?.items.get(0)?.data.user.id).toBe('u-1');
		expect(listState?.items.get(1)?.data.user.id).toBe('u-2');
		expect(listState?.items.get(2)).toBeUndefined();
		expect(listState?.items.size).toBe(2);
	});

	test('drops stale trailing rows after a full sync shrink', () => {
		const guildId = 'guild-5';
		const listId = 'list-5';
		seedMembers(guildId, [
			{id: 'u-1', name: 'Alpha'},
			{id: 'u-2', name: 'Bravo'},
			{id: 'u-3', name: 'Charlie'},
			{id: 'u-4', name: 'Delta'},
		]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 4,
			onlineCount: 4,
			groups: [{id: 'online', count: 4}],
			ops: [
				{
					op: 'SYNC',
					range: [0, 4],
					items: [
						{group: {id: 'online', count: 4}},
						{member: {user: {id: 'u-1'}}},
						{member: {user: {id: 'u-2'}}},
						{member: {user: {id: 'u-3'}}},
						{member: {user: {id: 'u-4'}}},
					],
				},
			],
		});

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 2,
			onlineCount: 2,
			groups: [{id: 'online', count: 2}],
			ops: [
				{
					op: 'SYNC',
					range: [0, 2],
					items: [{group: {id: 'online', count: 2}}, {member: {user: {id: 'u-1'}}}, {member: {user: {id: 'u-2'}}}],
				},
			],
		});

		const listState = MemberSidebarStore.getList(guildId, listId);
		expect(Array.from(listState?.rows.keys() ?? [])).toEqual([0, 1, 2]);
		expect(Array.from(listState?.items.values() ?? []).map((item) => item.data.user.id)).toEqual(['u-1', 'u-2']);
	});

	test('ignores list updates when member list updates are disabled for the guild', () => {
		const guildId = 'guild-disabled-updates';
		const listId = 'list-disabled-updates';
		GuildStore.guilds[guildId] = createGuild(guildId, GuildOperations.MEMBER_LIST_UPDATES);
		seedMembers(guildId, [{id: 'u-1', name: 'Alpha'}]);

		MemberSidebarStore.handleListUpdate({
			guildId,
			listId,
			memberCount: 1,
			onlineCount: 1,
			groups: [{id: 'online', count: 1}],
			ops: [
				{
					op: 'SYNC',
					range: [0, 1],
					items: [{group: {id: 'online', count: 1}}, {member: {user: {id: 'u-1'}}}],
				},
			],
		});

		expect(MemberSidebarStore.getList(guildId, listId)).toBeUndefined();
	});

	test('treats subscribe attempts as no-op when member list updates are disabled for the guild', () => {
		const guildId = 'guild-disabled-subscribe';
		const channelId = 'channel-disabled-subscribe';
		GuildStore.guilds[guildId] = createGuild(guildId, GuildOperations.MEMBER_LIST_UPDATES);

		MemberSidebarStore.subscribeToChannel(guildId, channelId, [[0, 99]]);

		expect(MemberSidebarStore.getList(guildId, channelId)).toBeUndefined();
		expect(MemberSidebarStore.getSubscribedRanges(guildId, channelId)).toEqual([]);
	});
});
