// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {describe, expect, it} from 'vitest';

function entryWith(fields: Record<string, unknown>): Record<string, unknown> {
	return {
		id: '1420000000000000100',
		action_type: AuditLogActionType.MEMBER_BAN_ADD,
		user_id: '1420000000000000001',
		target_id: '1420000000000000002',
		...fields,
	};
}

describe('GuildAuditLogEntryResponse', () => {
	it('parses delete_message_seconds as a number', () => {
		const parsed = GuildAuditLogEntryResponse.parse(entryWith({options: {delete_message_seconds: 3600}}));
		expect(parsed.options).toEqual({delete_message_seconds: 3600});
	});

	it('rejects delete_message_seconds sent as a string', () => {
		const result = GuildAuditLogEntryResponse.safeParse(entryWith({options: {delete_message_seconds: '3600'}}));
		expect(result.success).toBe(false);
	});

	it('parses the legacy delete_member_days string', () => {
		const parsed = GuildAuditLogEntryResponse.parse(entryWith({options: {delete_member_days: '7'}}));
		expect(parsed.options).toEqual({delete_member_days: '7'});
	});

	it('parses a permissions_diff change value', () => {
		const changes = [
			{key: 'permissions', old_value: '1024', new_value: '3072'},
			{key: 'permissions_diff', new_value: {added: ['SEND_MESSAGES'], removed: []}},
		];
		const parsed = GuildAuditLogEntryResponse.parse(
			entryWith({action_type: AuditLogActionType.ROLE_UPDATE, options: {role_name: 'Moderators'}, changes}),
		);
		expect(parsed.changes).toEqual(changes);
		expect(parsed.options).toEqual({role_name: 'Moderators'});
	});

	it('strips unknown option keys', () => {
		const parsed = GuildAuditLogEntryResponse.parse(
			entryWith({
				action_type: AuditLogActionType.MEMBER_UPDATE,
				options: {channel_id: '1420000000000000003', timeout_reason: 'raw', future_option: 1},
			}),
		);
		expect(parsed.options).toEqual({channel_id: '1420000000000000003'});
	});
});
