// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {
	collectGuildAuditLogUserIds,
	isNoopGuildAuditLog,
	mapGuildAuditLogEntry,
} from '@app/api/guild/GuildAuditLogEntryMapper';
import type {GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {describe, expect, it} from 'vitest';

const GUILD_ID = createGuildID(1420000000000000000n);
const ACTOR_ID = '1420000000000000001';
const TARGET_ID = '1420000000000000002';
const OTHER_ID = '1420000000000000003';

const ALL_ACTIONS = Object.values(AuditLogActionType).filter(
	(value): value is AuditLogActionType => typeof value === 'number',
);

const SKIPPABLE_ACTIONS = [
	AuditLogActionType.GUILD_UPDATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.ROLE_UPDATE,
	AuditLogActionType.WEBHOOK_UPDATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.STICKER_UPDATE,
];

const RECORDED_ACTIONS = ALL_ACTIONS.filter((action) => !SKIPPABLE_ACTIONS.includes(action));

function makeLog(params: {
	actionType: AuditLogActionType;
	userId?: string;
	targetId?: string | null;
	reason?: string | null;
	options?: Record<string, string>;
	changes?: GuildAuditLogChange | null;
}): GuildAuditLog {
	return new GuildAuditLog({
		guild_id: GUILD_ID,
		log_id: 1420000000000000100n,
		user_id: createUserID(BigInt(params.userId ?? ACTOR_ID)),
		target_id: params.targetId ?? null,
		action_type: params.actionType,
		reason: params.reason ?? null,
		options: params.options ? new Map(Object.entries(params.options)) : null,
		changes: params.changes ? JSON.stringify(params.changes) : null,
	});
}

describe('mapGuildAuditLogEntry', () => {
	it('maps the legacy delete_message_days key onto delete_member_days', () => {
		const legacyOnly = mapGuildAuditLogEntry(
			makeLog({actionType: AuditLogActionType.MEMBER_BAN_ADD, options: {delete_message_days: '2'}}),
		);
		expect(legacyOnly.options).toEqual({delete_member_days: '2'});
		const legacyFirst = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				options: {delete_message_days: '2', delete_member_days: '5'},
			}),
		);
		expect(legacyFirst.options).toEqual({delete_member_days: '5'});
		const currentFirst = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				options: {delete_member_days: '5', delete_message_days: '2'},
			}),
		);
		expect(currentFirst.options).toEqual({delete_member_days: '5'});
	});

	it('scrubs ip changes', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				changes: [
					{key: 'ip', new_value: '203.0.113.1'},
					{key: 'reason', new_value: 'spam'},
				],
			}),
		);
		expect(entry.changes).toEqual([{key: 'reason', new_value: 'spam'}]);
	});

	it('returns undefined changes for empty and ip-only rows', () => {
		expect(mapGuildAuditLogEntry(makeLog({actionType: AuditLogActionType.MEMBER_BAN_ADD, changes: []})).changes).toBe(
			undefined,
		);
		expect(
			mapGuildAuditLogEntry(
				makeLog({actionType: AuditLogActionType.MEMBER_BAN_ADD, changes: [{key: 'ip', new_value: '203.0.113.1'}]}),
			).changes,
		).toBeUndefined();
		expect(mapGuildAuditLogEntry(makeLog({actionType: AuditLogActionType.MEMBER_KICK})).changes).toBeUndefined();
	});

	it.each(ALL_ACTIONS)('maps action %i without throwing', (actionType) => {
		const log = makeLog({
			actionType,
			targetId: TARGET_ID,
			options: {channel_id: OTHER_ID, type: '1', inviter_id: OTHER_ID},
			changes: [
				{key: 'owner_id', new_value: OTHER_ID},
				{key: 'moderator_id', old_value: OTHER_ID},
				{key: 'creator_id', old_value: OTHER_ID},
			],
		});
		expect(() => mapGuildAuditLogEntry(log)).not.toThrow();
		expect(() => collectGuildAuditLogUserIds(log)).not.toThrow();
	});
});

describe('isNoopGuildAuditLog', () => {
	it.each(SKIPPABLE_ACTIONS)('treats action %i with only an ip change as a no-op', (actionType) => {
		expect(isNoopGuildAuditLog(actionType, [{key: 'ip', new_value: '203.0.113.1'}])).toBe(true);
	});

	it.each(RECORDED_ACTIONS)('never treats action %i as a no-op', (actionType) => {
		expect(isNoopGuildAuditLog(actionType, [])).toBe(false);
		expect(isNoopGuildAuditLog(actionType, null)).toBe(false);
	});
});
