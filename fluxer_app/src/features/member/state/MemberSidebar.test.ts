// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({default: {socket: null}}));
vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => null}}));
vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {
		getGuild: (guildId: string) => ({
			id: guildId,
			disabledOperations: 0,
			roles: {
				'10': {id: '10', permissions: 0n, position: 0},
				'20': {id: '20', permissions: 0n, position: 5},
				'30': {id: '30', permissions: 0n, position: 9},
			},
		}),
	},
}));

installVoiceMenuTestBootstrap();

const {canManageTargetUser} = await import('@app/features/permissions/utils/PermissionUtils');
const {default: GuildMembers} = await import('@app/features/member/state/GuildMembers');
const {default: MemberSidebar} = await import('@app/features/member/state/MemberSidebar');

const MODERATOR_ROLE = {id: '20', permissions: 0n, position: 5};

let nextGuildId = 0;

function makeMember(userId: string, roles: Array<string>, nick: string | null = null): GuildMemberData {
	return {
		user: {
			id: userId,
			username: `user-${userId}`,
			discriminator: '0001',
			global_name: null,
			avatar: null,
			avatar_color: null,
			flags: 0,
		},
		nick,
		roles,
		joined_at: '2026-01-01T00:00:00.000Z',
	};
}

function makeGuild(guildId: string): Guild {
	return {id: guildId, owner_id: 'owner'} as unknown as Guild;
}

function syncMemberList(guildId: string, channelId: string, members: Array<GuildMemberData>): void {
	MemberSidebar.subscribeToChannel(guildId, channelId, [[0, 99]]);
	MemberSidebar.handleListUpdate({
		guildId,
		listId: 'everyone',
		channelId,
		memberCount: members.length,
		onlineCount: members.length,
		groups: [{id: 'online', count: members.length}],
		ops: [
			{
				op: 'SYNC',
				range: [0, members.length],
				items: [{group: {id: 'online', count: members.length}}, ...members.map((member) => ({member}))],
			},
		],
	});
}

function setupGuild(members: Array<GuildMemberData>): string {
	const guildId = `guild-${++nextGuildId}`;
	syncMemberList(guildId, `channel-${guildId}`, members);
	return guildId;
}

describe('MemberSidebar member list hydration', () => {
	it('makes membership known for members that only ever appeared in the member list', () => {
		const guildId = setupGuild([makeMember('target-low', []), makeMember('target-high', ['30'])]);
		expect(GuildMembers.isMembershipKnown(guildId, 'target-low')).toBe(true);
		expect(GuildMembers.getMember(guildId, 'target-high')?.roles.has('30')).toBe(true);
	});

	it('lets the role hierarchy check pass for a sidebar member who has never posted', () => {
		const guildId = setupGuild([makeMember('target-low', [])]);
		expect(canManageTargetUser(makeGuild(guildId), 'me', MODERATOR_ROLE, 'target-low')).toBe(true);
	});

	it('still refuses a sidebar member who outranks you', () => {
		const guildId = setupGuild([makeMember('target-high', ['30'])]);
		expect(canManageTargetUser(makeGuild(guildId), 'me', MODERATOR_ROLE, 'target-high')).toBe(false);
	});

	it('does not create members for group header rows', () => {
		const guildId = setupGuild([makeMember('target-low', []), makeMember('target-high', ['30'])]);
		expect(GuildMembers.getMemberCount(guildId)).toBe(2);
	});

	it('leaves an already known member untouched', () => {
		const guildId = `guild-${++nextGuildId}`;
		GuildMembers.hydrateIfMissing(guildId, makeMember('target-low', ['20'], 'authoritative'));
		syncMemberList(guildId, `channel-${guildId}`, [makeMember('target-low', [], 'stale')]);
		expect(GuildMembers.getMember(guildId, 'target-low')?.nick).toBe('authoritative');
	});
});
