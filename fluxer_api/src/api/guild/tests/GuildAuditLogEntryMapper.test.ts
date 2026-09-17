// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {
	collectGuildAuditLogUserIds,
	GUILD_AUDIT_INTERNAL_CHANGE_KEYS,
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

const USER_TARGET_ACTIONS = [
	AuditLogActionType.MEMBER_KICK,
	AuditLogActionType.MEMBER_PRUNE,
	AuditLogActionType.MEMBER_BAN_ADD,
	AuditLogActionType.MEMBER_BAN_REMOVE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.MEMBER_DISCONNECT,
	AuditLogActionType.BOT_ADD,
];

const OVERWRITE_ACTIONS = [
	AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
];

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

function userIdStrings(log: GuildAuditLog): Array<string> {
	return collectGuildAuditLogUserIds(log).map((id) => id.toString());
}

describe('mapGuildAuditLogEntry', () => {
	it('keeps the identity fields', () => {
		const entry = mapGuildAuditLogEntry(makeLog({actionType: AuditLogActionType.MEMBER_KICK, targetId: TARGET_ID}));
		expect(entry.id).toBe('1420000000000000100');
		expect(entry.action_type).toBe(AuditLogActionType.MEMBER_KICK);
		expect(entry.user_id).toBe(ACTOR_ID);
		expect(entry.target_id).toBe(TARGET_ID);
	});

	it('types every schema option and drops unknown keys', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.INVITE_DELETE,
				options: {
					channel_id: OTHER_ID,
					count: '3',
					delete_member_days: '7',
					delete_message_seconds: '3600',
					id: TARGET_ID,
					integration_type: '1',
					message_id: OTHER_ID,
					members_removed: '4',
					role_name: 'Moderators',
					type: '1',
					inviter_id: ACTOR_ID,
					max_age: '86400',
					max_uses: '10',
					temporary: 'true',
					uses: '2',
					name: 'raw name',
					new_owner_id: OTHER_ID,
					vanity_url_code: 'fluxer',
					role_id: OTHER_ID,
					action: 'add',
					timeout_reason: 'raw reason',
					communication_disabled_until: '2026-09-13T00:00:00.000Z',
				},
			}),
		);
		expect(entry.options).toEqual({
			channel_id: OTHER_ID,
			count: 3,
			delete_member_days: '7',
			delete_message_seconds: 3600,
			id: TARGET_ID,
			integration_type: 1,
			message_id: OTHER_ID,
			members_removed: 4,
			role_name: 'Moderators',
			type: 1,
			inviter_id: ACTOR_ID,
			max_age: 86400,
			max_uses: 10,
			temporary: true,
			uses: 2,
		});
	});

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

	it('drops numeric options that do not parse', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				options: {count: 'many', delete_message_seconds: 'NaN'},
			}),
		);
		expect(entry.options).toBeUndefined();
	});

	it('returns undefined options when nothing survives', () => {
		expect(mapGuildAuditLogEntry(makeLog({actionType: AuditLogActionType.GUILD_UPDATE})).options).toBeUndefined();
		expect(
			mapGuildAuditLogEntry(makeLog({actionType: AuditLogActionType.GUILD_UPDATE, options: {name: 'Fluxer'}})).options,
		).toBeUndefined();
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

	it('keeps an explicit reason trimmed', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({actionType: AuditLogActionType.MEMBER_KICK, targetId: TARGET_ID, reason: '  spam  '}),
		);
		expect(entry.reason).toBe('spam');
	});

	it('returns undefined for a blank reason', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({actionType: AuditLogActionType.MEMBER_KICK, targetId: TARGET_ID, reason: '   '}),
		);
		expect(entry.reason).toBeUndefined();
	});

	it('falls back to the ban change reason', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				targetId: TARGET_ID,
				changes: [{key: 'reason', new_value: 'body reason'}],
			}),
		);
		expect(entry.reason).toBe('body reason');
	});

	it('prefers the explicit reason over the ban change reason', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				targetId: TARGET_ID,
				reason: 'header reason',
				changes: [{key: 'reason', new_value: 'body reason'}],
			}),
		);
		expect(entry.reason).toBe('header reason');
	});

	it('ignores a blank ban change reason', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_BAN_ADD,
				targetId: TARGET_ID,
				reason: ' ',
				changes: [{key: 'reason', new_value: '   '}],
			}),
		);
		expect(entry.reason).toBeUndefined();
	});

	it('falls back to the timeout_reason option for member updates', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_UPDATE,
				targetId: TARGET_ID,
				options: {timeout_reason: 'cool off', communication_disabled_until: '2026-09-13T00:00:00.000Z'},
				changes: [{key: 'communication_disabled_until', new_value: '2026-09-13T00:00:00.000Z'}],
			}),
		);
		expect(entry.reason).toBe('cool off');
		expect(entry.options).toBeUndefined();
	});

	it('ignores a blank timeout_reason option', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({actionType: AuditLogActionType.MEMBER_UPDATE, targetId: TARGET_ID, options: {timeout_reason: '  '}}),
		);
		expect(entry.reason).toBeUndefined();
	});

	it('never falls back for other actions', () => {
		const entry = mapGuildAuditLogEntry(
			makeLog({
				actionType: AuditLogActionType.MEMBER_KICK,
				targetId: TARGET_ID,
				options: {timeout_reason: 'cool off'},
				changes: [{key: 'reason', new_value: 'body reason'}],
			}),
		);
		expect(entry.reason).toBeUndefined();
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

describe('collectGuildAuditLogUserIds', () => {
	it('includes the actor and never a channel target', () => {
		const log = makeLog({actionType: AuditLogActionType.CHANNEL_CREATE, targetId: TARGET_ID});
		expect(userIdStrings(log)).toEqual([ACTOR_ID]);
	});

	it.each(USER_TARGET_ACTIONS)('includes the target of user action %i', (actionType) => {
		const log = makeLog({actionType, targetId: TARGET_ID});
		expect(userIdStrings(log)).toEqual([ACTOR_ID, TARGET_ID]);
	});

	it('includes the new owner of a guild update', () => {
		const log = makeLog({
			actionType: AuditLogActionType.GUILD_UPDATE,
			targetId: GUILD_ID.toString(),
			changes: [{key: 'owner_id', old_value: TARGET_ID, new_value: OTHER_ID}],
		});
		expect(userIdStrings(log)).toEqual([ACTOR_ID, OTHER_ID]);
	});

	it('includes the original moderator of an unban', () => {
		const log = makeLog({
			actionType: AuditLogActionType.MEMBER_BAN_REMOVE,
			targetId: TARGET_ID,
			changes: [{key: 'moderator_id', old_value: OTHER_ID}],
		});
		expect(userIdStrings(log)).toEqual([ACTOR_ID, TARGET_ID, OTHER_ID]);
	});

	it('includes the inviter of a deleted invite', () => {
		const log = makeLog({
			actionType: AuditLogActionType.INVITE_DELETE,
			targetId: 'abcdef',
			options: {inviter_id: OTHER_ID},
		});
		expect(userIdStrings(log)).toEqual([ACTOR_ID, OTHER_ID]);
	});

	it('does not include the inviter of a created invite', () => {
		const log = makeLog({
			actionType: AuditLogActionType.INVITE_CREATE,
			targetId: 'abcdef',
			options: {inviter_id: OTHER_ID},
		});
		expect(userIdStrings(log)).toEqual([ACTOR_ID]);
	});

	it.each([AuditLogActionType.WEBHOOK_DELETE, AuditLogActionType.EMOJI_DELETE, AuditLogActionType.STICKER_DELETE])(
		'includes the creator of deleted content for action %i',
		(actionType) => {
			const log = makeLog({
				actionType,
				targetId: TARGET_ID,
				changes: [
					{key: 'name', old_value: 'blob'},
					{key: 'creator_id', old_value: OTHER_ID},
				],
			});
			expect(userIdStrings(log)).toEqual([ACTOR_ID, OTHER_ID]);
		},
	);

	it.each(OVERWRITE_ACTIONS)('includes member overwrite targets for action %i', (actionType) => {
		const memberLog = makeLog({actionType, targetId: TARGET_ID, options: {type: '1', channel_id: OTHER_ID}});
		expect(userIdStrings(memberLog)).toEqual([ACTOR_ID, TARGET_ID]);
		const roleLog = makeLog({actionType, targetId: TARGET_ID, options: {type: '0', channel_id: OTHER_ID}});
		expect(userIdStrings(roleLog)).toEqual([ACTOR_ID]);
	});

	it('skips values that are not snowflakes', () => {
		expect(userIdStrings(makeLog({actionType: AuditLogActionType.MEMBER_KICK, targetId: 'not-a-snowflake'}))).toEqual([
			ACTOR_ID,
		]);
		expect(userIdStrings(makeLog({actionType: AuditLogActionType.MEMBER_KICK, targetId: '-1'}))).toEqual([ACTOR_ID]);
		expect(
			userIdStrings(makeLog({actionType: AuditLogActionType.MEMBER_KICK, targetId: '99999999999999999999'})),
		).toEqual([ACTOR_ID]);
		const numericOwner = makeLog({
			actionType: AuditLogActionType.GUILD_UPDATE,
			changes: [{key: 'owner_id', new_value: 1420}],
		});
		expect(userIdStrings(numericOwner)).toEqual([ACTOR_ID]);
		const missingCreator = makeLog({
			actionType: AuditLogActionType.EMOJI_DELETE,
			changes: [{key: 'creator_id', old_value: null}],
		});
		expect(userIdStrings(missingCreator)).toEqual([ACTOR_ID]);
	});

	it('collapses duplicates', () => {
		const selfUpdate = makeLog({actionType: AuditLogActionType.MEMBER_UPDATE, targetId: ACTOR_ID});
		expect(userIdStrings(selfUpdate)).toEqual([ACTOR_ID]);
		const unbanOfModerator = makeLog({
			actionType: AuditLogActionType.MEMBER_BAN_REMOVE,
			targetId: TARGET_ID,
			changes: [{key: 'moderator_id', old_value: TARGET_ID}],
		});
		expect(userIdStrings(unbanOfModerator)).toEqual([ACTOR_ID, TARGET_ID]);
	});
});

describe('isNoopGuildAuditLog', () => {
	it.each(SKIPPABLE_ACTIONS)('treats action %i with empty or null changes as a no-op', (actionType) => {
		expect(isNoopGuildAuditLog(actionType, [])).toBe(true);
		expect(isNoopGuildAuditLog(actionType, null)).toBe(true);
		expect(isNoopGuildAuditLog(actionType, undefined)).toBe(true);
	});

	it.each(SKIPPABLE_ACTIONS)('treats action %i with only an ip change as a no-op', (actionType) => {
		expect(isNoopGuildAuditLog(actionType, [{key: 'ip', new_value: '203.0.113.1'}])).toBe(true);
	});

	it.each(SKIPPABLE_ACTIONS)('keeps action %i with a real change', (actionType) => {
		expect(isNoopGuildAuditLog(actionType, [{key: 'name', old_value: 'a', new_value: 'b'}])).toBe(false);
	});

	it('lists the internal guild change keys', () => {
		expect([...GUILD_AUDIT_INTERNAL_CHANGE_KEYS].sort()).toEqual(
			[
				'guild_id',
				'member_count',
				'banner_width',
				'banner_height',
				'splash_width',
				'splash_height',
				'embed_splash_width',
				'embed_splash_height',
			].sort(),
		);
	});

	it.each([...GUILD_AUDIT_INTERNAL_CHANGE_KEYS])('treats a guild update of only %s as a no-op', (key) => {
		expect(isNoopGuildAuditLog(AuditLogActionType.GUILD_UPDATE, [{key, old_value: 1, new_value: 2}])).toBe(true);
	});

	it('treats a dimension-only guild update as a no-op', () => {
		expect(
			isNoopGuildAuditLog(AuditLogActionType.GUILD_UPDATE, [
				{key: 'banner_width', old_value: 960, new_value: 1920},
				{key: 'banner_height', old_value: 540, new_value: 1080},
				{key: 'ip', new_value: '203.0.113.1'},
			]),
		).toBe(true);
	});

	it('keeps a guild update that renames the guild', () => {
		expect(
			isNoopGuildAuditLog(AuditLogActionType.GUILD_UPDATE, [
				{key: 'banner_width', old_value: 960, new_value: 1920},
				{key: 'name', old_value: 'Old', new_value: 'New'},
			]),
		).toBe(false);
	});

	it('only ignores internal keys for guild updates', () => {
		expect(
			isNoopGuildAuditLog(AuditLogActionType.MEMBER_UPDATE, [{key: 'banner_width', old_value: 1, new_value: 2}]),
		).toBe(false);
	});

	it('treats a member move without changes as a no-op but keeps a disconnect', () => {
		expect(isNoopGuildAuditLog(AuditLogActionType.MEMBER_MOVE, null)).toBe(true);
		expect(isNoopGuildAuditLog(AuditLogActionType.MEMBER_DISCONNECT, null)).toBe(false);
	});

	it.each(RECORDED_ACTIONS)('never treats action %i as a no-op', (actionType) => {
		expect(isNoopGuildAuditLog(actionType, [])).toBe(false);
		expect(isNoopGuildAuditLog(actionType, null)).toBe(false);
	});
});
