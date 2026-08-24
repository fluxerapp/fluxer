// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import {MEMBER_LIST_RANGE_MAX_END, MEMBER_LIST_RANGE_MAX_SPAN} from '@fluxer/constants/src/GatewayConstants';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

interface MemberListGroup {
	id: string;
	count: number;
}

interface MemberListPresence {
	status?: string;
	custom_status?: GatewayCustomStatusPayload | null;
}

interface MemberListItem {
	member?: GuildMemberData & {
		presence?: MemberListPresence | null;
	};
	group?: MemberListGroup;
}

interface MemberListSyncOperation {
	op: 'SYNC';
	range: [number, number];
	items: ReadonlyArray<MemberListItem>;
}

interface MemberListIncomingOperation {
	op?: string;
	range?: [number, number];
	items?: ReadonlyArray<MemberListItem>;
}

interface GuildMemberListUpdatePayload {
	guild_id: string;
	id: string;
	channel_id?: string;
	member_count: number;
	online_count: number;
	groups: ReadonlyArray<MemberListGroup>;
	ops: ReadonlyArray<MemberListIncomingOperation>;
}

function isValidRange(range: [number, number] | undefined): range is [number, number] {
	if (!range || !Array.isArray(range) || range.length !== 2) {
		return false;
	}
	const [start, end] = range;
	return (
		typeof start === 'number' &&
		typeof end === 'number' &&
		start >= 0 &&
		end >= start &&
		end <= MEMBER_LIST_RANGE_MAX_END &&
		end - start <= MEMBER_LIST_RANGE_MAX_SPAN
	);
}

export function handleGuildMemberListUpdate(data: GuildMemberListUpdatePayload, _context: GatewayHandlerContext): void {
	const {
		guild_id: guildId,
		id: listId,
		channel_id: channelId,
		member_count: memberCount,
		online_count: onlineCount,
		groups,
		ops,
	} = data;
	if (!guildId || !listId || !Array.isArray(ops)) {
		return;
	}
	const validOps: Array<MemberListSyncOperation> = [];
	for (const op of ops) {
		if (op.op !== 'SYNC' || !isValidRange(op.range) || !Array.isArray(op.items)) {
			continue;
		}
		validOps.push({
			op: 'SYNC',
			range: op.range,
			items: op.items,
		});
	}
	if (validOps.length === 0 && ops.length > 0) {
		return;
	}
	const safeGroups = Array.isArray(groups) ? Array.from(groups) : [];
	MemberSidebar.handleListUpdate({
		guildId,
		listId,
		channelId,
		memberCount,
		onlineCount,
		groups: safeGroups,
		ops: validOps,
	});
}
