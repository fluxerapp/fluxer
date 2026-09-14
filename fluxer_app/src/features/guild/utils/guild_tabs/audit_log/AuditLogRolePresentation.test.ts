// SPDX-License-Identifier: AGPL-3.0-or-later

import {presentAuditLogEntry} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentation';
import {
	presentRoleCreate,
	presentRoleDelete,
	presentRoleUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogRolePresentation';
import {
	fakeContext,
	makeEntry,
	resultToText,
	TEST_ACTOR_ID,
	TEST_GUILD_ID,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogTestUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

type EntryFixture = Parameters<typeof makeEntry>[0];
type ChangesFixture = NonNullable<EntryFixture['changes']>;

const ROLE_ID = '1400000000000000200';
const USER_NAMES = {[TEST_ACTOR_ID]: 'Hampus'};
const ORANGE = 0xe67e22;
const BLUE = 0x3498db;
const UNKNOWN_PERMISSION_BIT = 1n << 19n;

const APP_CREATE_SNAPSHOT: Record<string, unknown> = {
	role_id: ROLE_ID,
	name: 'New role',
	permissions: DEFAULT_PERMISSIONS.toString(),
	position: 1,
	hoist_position: null,
	color: 0,
	icon_hash: null,
	unicode_emoji: null,
	hoist: false,
	mentionable: false,
};

const DELETE_SNAPSHOT: Record<string, unknown> = {
	role_id: ROLE_ID,
	name: 'Mods',
	permissions: (Permissions.KICK_MEMBERS | Permissions.BAN_MEMBERS | Permissions.MANAGE_MESSAGES).toString(),
	position: 3,
	hoist_position: null,
	color: ORANGE,
	icon_hash: null,
	unicode_emoji: '🔥',
	hoist: true,
	mentionable: false,
};

function present(fixture: EntryFixture) {
	const presentation = presentAuditLogEntry(makeEntry(fixture), fakeContext());
	return {...resultToText(presentation, USER_NAMES), expandable: presentation.expandable};
}

function createdRole(snapshot: Record<string, unknown>, fixture: Partial<EntryFixture> = {}) {
	return present({
		action_type: AuditLogActionType.ROLE_CREATE,
		target_id: ROLE_ID,
		changes: Object.entries({...APP_CREATE_SNAPSHOT, ...snapshot}).map(([key, value]) => ({key, new_value: value})),
		...fixture,
	});
}

function updatedRole(changes: ChangesFixture, fixture: Partial<EntryFixture> = {}) {
	return present({
		action_type: AuditLogActionType.ROLE_UPDATE,
		target_id: ROLE_ID,
		options: {role_name: 'Mods'},
		changes,
		...fixture,
	});
}

function deletedRole(snapshot: Record<string, unknown>, fixture: Partial<EntryFixture> = {}) {
	return present({
		action_type: AuditLogActionType.ROLE_DELETE,
		target_id: ROLE_ID,
		changes: Object.entries({...DELETE_SNAPSHOT, ...snapshot}).map(([key, value]) => ({key, old_value: value})),
		...fixture,
	});
}

const GENERIC_UPDATE_WITHOUT_ROWS = {
	summary: 'Hampus updated the role @Mods.',
	rows: [],
	blocks: [],
	expandable: false,
};

describe('presentRoleCreate', () => {
	it('shows one Granted row for a role created from the app', () => {
		expect(createdRole({})).toEqual({
			summary: 'Hampus created the role @New role.',
			rows: [
				'+ Granted Create invite links, Add reactions, Stream video, View channel, Send messages, Embed links, Attach files, and 8 more permissions.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('shows nothing to expand for a role created without permissions', () => {
		expect(createdRole({permissions: '0'})).toEqual({
			summary: 'Hampus created the role @New role.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each([
		{label: 'a color', snapshot: {color: ORANGE}, row: '~ Set the role color to #E67E22.'},
		{
			label: 'members displayed separately',
			snapshot: {hoist: true},
			row: '+ Showed members with this role in their own section in the member list.',
		},
		{label: 'mentions allowed', snapshot: {mentionable: true}, row: '+ Allowed all members to mention this role.'},
		{label: 'an emoji icon', snapshot: {unicode_emoji: '🔥'}, row: '+ Set the role icon to 🔥.'},
		{label: 'a custom icon', snapshot: {icon_hash: 'a1b2c3'}, row: '+ Set a custom role icon.'},
	])('shows a row for $label', ({snapshot, row}) => {
		expect(createdRole({permissions: '0', ...snapshot})).toEqual({
			summary: 'Hampus created the role @New role.',
			rows: [row],
			blocks: [],
			expandable: true,
		});
	});

	it('orders every row of a fully customized role', () => {
		expect(
			createdRole({
				name: 'Moderators',
				permissions: (Permissions.MANAGE_MESSAGES | Permissions.SEND_MESSAGES).toString(),
				color: BLUE,
				icon_hash: 'a1b2c3',
				unicode_emoji: '🛡️',
				hoist: true,
				mentionable: true,
			}),
		).toEqual({
			summary: 'Hampus created the role @Moderators.',
			rows: [
				'+ Granted Send messages and Manage messages.',
				'~ Set the role color to #3498DB.',
				'+ Showed members with this role in their own section in the member list.',
				'+ Allowed all members to mention this role.',
				'+ Set the role icon to 🛡️.',
				'+ Set a custom role icon.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('reads gateway string and number forms', () => {
		expect(
			createdRole({
				permissions: Number(Permissions.SEND_MESSAGES),
				color: String(ORANGE),
				hoist: 'true',
				mentionable: 'false',
			}),
		).toEqual({
			summary: 'Hampus created the role @New role.',
			rows: [
				'+ Granted Send messages.',
				'~ Set the role color to #E67E22.',
				'+ Showed members with this role in their own section in the member list.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it.each([
		{permissions: '12abc'},
		{permissions: '-4'},
		{permissions: String(UNKNOWN_PERMISSION_BIT)},
		{color: -1},
		{color: 1.5},
		{color: 0x1000000},
		{color: 'orange'},
		{hoist: 'yes'},
		{mentionable: 1},
		{unicode_emoji: '   '},
		{icon_hash: 42},
		{icon_hash: ''},
	])('omits the row for %o', (snapshot) => {
		expect(createdRole({permissions: '0', ...snapshot}).rows).toEqual([]);
	});

	it('adds the decoded Reason block', () => {
		expect(createdRole({permissions: '0'}, {reason: 'Event%20staff'})).toEqual({
			summary: 'Hampus created the role @New role.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Event staff'}],
			expandable: true,
		});
	});

	it('leaves the recorded name empty when the name does not read', () => {
		expect(createdRole({name: '  '}).summary).toBe(`Hampus created the role @${ROLE_ID}.`);
		expect(
			presentRoleCreate(makeEntry({action_type: AuditLogActionType.ROLE_CREATE, target_id: 'role', changes: []}))
				.summary.values.role,
		).toEqual({kind: 'role', id: '', recordedName: null});
	});
});

describe('presentRoleUpdate', () => {
	it.each([
		{direction: 'up', oldValue: 2, newValue: 5, summary: 'Hampus moved the role @Mods up in the role list.'},
		{direction: 'down', oldValue: 5, newValue: 2, summary: 'Hampus moved the role @Mods down in the role list.'},
	])('summarizes a role moved $direction in the role list', ({oldValue, newValue, summary}) => {
		expect(updatedRole([{key: 'position', old_value: oldValue, new_value: newValue}])).toEqual({
			summary,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('uses the generic summary for a position that did not change', () => {
		expect(updatedRole([{key: 'position', old_value: 3, new_value: 3}])).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
	});

	it.each([
		{label: 'set', oldValue: null, newValue: 4, summary: 'Hampus set a member list position for the role @Mods.'},
		{label: 'reset', oldValue: 4, newValue: null, summary: 'Hampus reset the member list position for the role @Mods.'},
		{label: 'moved up', oldValue: 2, newValue: 5, summary: 'Hampus moved the role @Mods up in the member list.'},
		{label: 'moved down', oldValue: 5, newValue: 2, summary: 'Hampus moved the role @Mods down in the member list.'},
	])('summarizes a member list position that was $label', ({oldValue, newValue, summary}) => {
		expect(updatedRole([{key: 'hoist_position', old_value: oldValue, new_value: newValue}])).toEqual({
			summary,
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each([
		{oldValue: 3, newValue: 3},
		{oldValue: null, newValue: null},
		{oldValue: 'top', newValue: 3},
		{oldValue: 3, newValue: 'bottom'},
	])('ignores a member list position from $oldValue to $newValue', ({oldValue, newValue}) => {
		expect(updatedRole([{key: 'hoist_position', old_value: oldValue, new_value: newValue}])).toEqual(
			GENERIC_UPDATE_WITHOUT_ROWS,
		);
	});

	it('summarizes a rename with no rows', () => {
		expect(
			updatedRole([{key: 'name', old_value: 'Mods', new_value: 'Moderators'}], {options: {role_name: 'Moderators'}}),
		).toEqual({
			summary: 'Hampus renamed the role Mods to Moderators.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it('uses the generic summary for an entry with no changes', () => {
		expect(updatedRole([])).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
		expect(
			present({action_type: AuditLogActionType.ROLE_UPDATE, target_id: ROLE_ID, options: {role_name: 'Mods'}}),
		).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
	});

	it.each([
		{
			label: 'a granted permission',
			previous: Permissions.VIEW_CHANNEL,
			next: Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES,
			diff: {added: ['SEND_MESSAGES'], removed: []},
			rows: ['+ Granted Send messages.'],
		},
		{
			label: 'a revoked permission',
			previous: Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES,
			next: Permissions.VIEW_CHANNEL,
			diff: {added: [], removed: ['SEND_MESSAGES']},
			rows: ['- Revoked Send messages.'],
		},
		{
			label: 'granted and revoked permissions',
			previous: Permissions.KICK_MEMBERS,
			next: Permissions.BAN_MEMBERS | Permissions.MANAGE_MESSAGES,
			diff: {added: ['BAN_MEMBERS', 'MANAGE_MESSAGES'], removed: ['KICK_MEMBERS']},
			rows: ['+ Granted Ban members and Manage messages.', '- Revoked Kick members.'],
		},
	])('shows $label from the permissions pair and not permissions_diff', ({previous, next, diff, rows}) => {
		expect(
			updatedRole([
				{key: 'permissions', old_value: previous.toString(), new_value: next.toString()},
				{key: 'permissions_diff', new_value: diff},
			]),
		).toEqual({summary: 'Hampus updated the role @Mods.', rows, blocks: [], expandable: true});
	});

	it('reads legacy permissions_diff names when the permissions pair is absent', () => {
		expect(
			updatedRole([
				{
					key: 'permissions_diff',
					new_value: {added: ['MANAGE_MESSAGES', 'SEND_MESSAGES', 'NOT_A_PERMISSION'], removed: ['KICK_MEMBERS']},
				},
			]),
		).toEqual({
			summary: 'Hampus updated the role @Mods.',
			rows: ['+ Granted Send messages and Manage messages.', '- Revoked Kick members.'],
			blocks: [],
			expandable: true,
		});
	});

	it('does not fall back to permissions_diff when the permissions pair does not read', () => {
		expect(
			updatedRole([
				{key: 'permissions', old_value: 'abc', new_value: Permissions.VIEW_CHANNEL.toString()},
				{key: 'permissions_diff', new_value: {added: ['VIEW_CHANNEL'], removed: []}},
			]),
		).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
	});

	it('shows nothing for permissions that differ only in bits the app does not know', () => {
		expect(
			updatedRole([
				{
					key: 'permissions',
					old_value: Permissions.VIEW_CHANNEL.toString(),
					new_value: (Permissions.VIEW_CHANNEL | UNKNOWN_PERMISSION_BIT).toString(),
				},
			]),
		).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
	});

	it.each([
		{label: 'set', oldValue: 0, newValue: ORANGE, row: '+ Set the role color to #E67E22.'},
		{label: 'removed', oldValue: ORANGE, newValue: 0, row: '- Removed the role color.'},
		{label: 'changed', oldValue: ORANGE, newValue: BLUE, row: '~ Changed the role color from #E67E22 to #3498DB.'},
	])('shows a role color that was $label', ({oldValue, newValue, row}) => {
		expect(updatedRole([{key: 'color', old_value: oldValue, new_value: newValue}])).toEqual({
			summary: 'Hampus updated the role @Mods.',
			rows: [row],
			blocks: [],
			expandable: true,
		});
	});

	it.each([
		{key: 'hoist', oldValue: false, newValue: true, row: '+ Started displaying members with this role separately.'},
		{key: 'hoist', oldValue: true, newValue: false, row: '- Stopped displaying members with this role separately.'},
		{key: 'mentionable', oldValue: false, newValue: true, row: '+ Allowed all members to mention this role.'},
		{
			key: 'mentionable',
			oldValue: true,
			newValue: false,
			row: '- Limited mentions of this role to members with permission to mention any role.',
		},
		{key: 'unicode_emoji', oldValue: null, newValue: '🔥', row: '+ Set the role icon to 🔥.'},
		{key: 'unicode_emoji', oldValue: '🔥', newValue: '⭐', row: '~ Changed the role icon from 🔥 to ⭐.'},
		{key: 'unicode_emoji', oldValue: '🔥', newValue: null, row: '- Removed the role icon.'},
		{key: 'icon_hash', oldValue: null, newValue: 'a1b2c3', row: '+ Set a custom role icon.'},
		{key: 'icon_hash', oldValue: 'a1b2c3', newValue: 'd4e5f6', row: '~ Changed the custom role icon.'},
		{key: 'icon_hash', oldValue: 'a1b2c3', newValue: null, row: '- Removed the custom role icon.'},
	])('shows $key from $oldValue to $newValue', ({key, oldValue, newValue, row}) => {
		expect(updatedRole([{key, old_value: oldValue, new_value: newValue}])).toEqual({
			summary: 'Hampus updated the role @Mods.',
			rows: [row],
			blocks: [],
			expandable: true,
		});
	});

	it('treats blank icons as no icon', () => {
		expect(
			updatedRole([
				{key: 'unicode_emoji', old_value: '  ', new_value: null},
				{key: 'icon_hash', old_value: null, new_value: ''},
			]),
		).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
	});

	it.each([
		{change: {key: 'position', old_value: 2, new_value: 5}, row: '~ Moved the role up in the role list.'},
		{change: {key: 'position', old_value: 5, new_value: 2}, row: '~ Moved the role down in the role list.'},
		{change: {key: 'hoist_position', old_value: null, new_value: 4}, row: '~ Set a member list position for the role.'},
		{
			change: {key: 'hoist_position', old_value: 4, new_value: null},
			row: '~ Reset the member list position for the role.',
		},
		{change: {key: 'hoist_position', old_value: 2, new_value: 5}, row: '~ Moved the role up in the member list.'},
		{change: {key: 'hoist_position', old_value: 5, new_value: 2}, row: '~ Moved the role down in the member list.'},
	])('shows $change.key as a row when it comes with another change', ({change, row}) => {
		expect(updatedRole([change, {key: 'color', old_value: 0, new_value: ORANGE}])).toEqual({
			summary: 'Hampus updated the role @Mods.',
			rows: ['+ Set the role color to #E67E22.', row],
			blocks: [],
			expandable: true,
		});
	});

	it('lists every change in a fixed order under the generic summary', () => {
		expect(
			updatedRole(
				[
					{key: 'hoist_position', old_value: null, new_value: 3},
					{key: 'position', old_value: 2, new_value: 4},
					{key: 'icon_hash', old_value: 'a1b2c3', new_value: null},
					{key: 'unicode_emoji', old_value: null, new_value: '🔥'},
					{key: 'mentionable', old_value: true, new_value: false},
					{key: 'hoist', old_value: false, new_value: true},
					{key: 'color', old_value: ORANGE, new_value: BLUE},
					{key: 'permissions_diff', new_value: {added: ['MANAGE_MESSAGES'], removed: ['KICK_MEMBERS']}},
					{
						key: 'permissions',
						old_value: Permissions.KICK_MEMBERS.toString(),
						new_value: Permissions.MANAGE_MESSAGES.toString(),
					},
					{key: 'name', old_value: 'Mods', new_value: 'Moderators'},
					{key: 'role_id', new_value: ROLE_ID},
				],
				{options: {role_name: 'Moderators'}},
			),
		).toEqual({
			summary: 'Hampus updated the role @Moderators.',
			rows: [
				'~ Changed the name from Mods to Moderators.',
				'+ Granted Manage messages.',
				'- Revoked Kick members.',
				'~ Changed the role color from #E67E22 to #3498DB.',
				'+ Started displaying members with this role separately.',
				'- Limited mentions of this role to members with permission to mention any role.',
				'+ Set the role icon to 🔥.',
				'- Removed the custom role icon.',
				'~ Moved the role up in the role list.',
				'~ Set a member list position for the role.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('names the role by its new name before the recorded role name', () => {
		expect(
			updatedRole(
				[
					{key: 'name', old_value: 'Mods', new_value: 'Moderators'},
					{key: 'hoist', old_value: false, new_value: true},
				],
				{options: {role_name: 'Staff'}},
			),
		).toEqual({
			summary: 'Hampus updated the role @Moderators.',
			rows: ['~ Changed the name from Mods to Moderators.', '+ Started displaying members with this role separately.'],
			blocks: [],
			expandable: true,
		});
	});

	it('shows @everyone for the default role', () => {
		const entry = makeEntry({
			action_type: AuditLogActionType.ROLE_UPDATE,
			target_id: TEST_GUILD_ID,
			options: {role_name: '@everyone'},
			changes: [
				{
					key: 'permissions',
					old_value: Permissions.VIEW_CHANNEL.toString(),
					new_value: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
				},
			],
		});
		expect(presentRoleUpdate(entry).summary.values.role).toEqual({
			kind: 'role',
			id: TEST_GUILD_ID,
			recordedName: '@everyone',
		});
		expect(present({...entry, action_type: AuditLogActionType.ROLE_UPDATE})).toEqual({
			summary: 'Hampus updated the role @everyone.',
			rows: ['+ Granted Send messages.'],
			blocks: [],
			expandable: true,
		});
	});

	it('names a deleted role by the recorded role name', () => {
		expect(updatedRole([{key: 'position', old_value: 1, new_value: 3}], {options: {role_name: 'Old staff'}})).toEqual({
			summary: 'Hampus moved the role @Old staff up in the role list.',
			rows: [],
			blocks: [],
			expandable: false,
		});
		expect(
			presentRoleUpdate(
				makeEntry({
					action_type: AuditLogActionType.ROLE_UPDATE,
					target_id: ROLE_ID,
					changes: [{key: 'position', old_value: 1, new_value: 3}],
				}),
			).summary.values.role,
		).toEqual({kind: 'role', id: ROLE_ID, recordedName: null});
	});

	it('reads gateway string forms', () => {
		expect(
			updatedRole([
				{key: 'color', old_value: '0', new_value: String(ORANGE)},
				{key: 'hoist', old_value: 'false', new_value: 'true'},
				{key: 'position', old_value: '4', new_value: '2'},
			]),
		).toEqual({
			summary: 'Hampus updated the role @Mods.',
			rows: [
				'+ Set the role color to #E67E22.',
				'+ Started displaying members with this role separately.',
				'~ Moved the role down in the role list.',
			],
			blocks: [],
			expandable: true,
		});
	});

	it('omits changes whose values do not read', () => {
		expect(
			updatedRole(
				[
					{key: 'name', old_value: 42, new_value: 'Mods'},
					{key: 'permissions', old_value: '-1', new_value: Permissions.VIEW_CHANNEL.toString()},
					{key: 'color', old_value: 'red', new_value: ORANGE},
					{key: 'hoist', old_value: 'yes', new_value: true},
					{key: 'mentionable', new_value: true},
					{key: 'unicode_emoji', old_value: 5, new_value: '🔥'},
					{key: 'icon_hash', old_value: null, new_value: {}},
					{key: 'position', old_value: null, new_value: 3},
					{key: 'hoist_position', old_value: 'top', new_value: 3},
				],
				{options: {role_name: 'Staff'}},
			),
		).toEqual(GENERIC_UPDATE_WITHOUT_ROWS);
	});

	it('adds the decoded Reason block to a summary-only entry', () => {
		expect(updatedRole([{key: 'position', old_value: 2, new_value: 5}], {reason: 'Reorder%20pass'})).toEqual({
			summary: 'Hampus moved the role @Mods up in the role list.',
			rows: [],
			blocks: [{kind: 'reason', text: 'Reorder pass'}],
			expandable: true,
		});
	});

	it('names the system as the actor', () => {
		expect(updatedRole([{key: 'position', old_value: 2, new_value: 5}], {user_id: '0'}).summary).toBe(
			'System moved the role @Mods up in the role list.',
		);
	});
});

describe('presentRoleDelete', () => {
	it('lists the permissions the deleted role granted', () => {
		expect(deletedRole({})).toEqual({
			summary: 'Hampus deleted the role @Mods.',
			rows: ['- The role granted Kick members, Ban members, and Manage messages.'],
			blocks: [],
			expandable: true,
		});
	});

	it('shows nothing to expand for a deleted role without permissions', () => {
		expect(deletedRole({permissions: '0'})).toEqual({
			summary: 'Hampus deleted the role @Mods.',
			rows: [],
			blocks: [],
			expandable: false,
		});
	});

	it.each([String(UNKNOWN_PERMISSION_BIT), 'abc', null])('shows no row for the permissions %o', (permissions) => {
		expect(deletedRole({permissions}).rows).toEqual([]);
	});

	it('caps a long permission list', () => {
		expect(deletedRole({permissions: DEFAULT_PERMISSIONS.toString()}).rows).toEqual([
			'- The role granted Create invite links, Add reactions, Stream video, View channel, Send messages, Embed links, Attach files, and 8 more permissions.',
		]);
	});

	it('adds the decoded Reason block', () => {
		expect(deletedRole({permissions: '0'}, {reason: 'No%20longer%20needed'})).toEqual({
			summary: 'Hampus deleted the role @Mods.',
			rows: [],
			blocks: [{kind: 'reason', text: 'No longer needed'}],
			expandable: true,
		});
	});

	it('leaves the recorded name empty when the name does not read', () => {
		expect(deletedRole({name: null}).summary).toBe(`Hampus deleted the role @${ROLE_ID}.`);
	});
});

describe('role presenters with junk entries', () => {
	it.each([
		{presenter: presentRoleCreate, actionType: AuditLogActionType.ROLE_CREATE},
		{presenter: presentRoleUpdate, actionType: AuditLogActionType.ROLE_UPDATE},
		{presenter: presentRoleDelete, actionType: AuditLogActionType.ROLE_DELETE},
	])('never throws and shows no rows for action type $actionType', ({presenter, actionType}) => {
		const entry = makeEntry({
			action_type: actionType,
			target_id: 'role',
			user_id: null,
			options: {role_name: 7},
			changes: [
				{key: 'name'},
				{key: 'permissions', old_value: [], new_value: {added: 1}},
				{key: 'permissions_diff', new_value: {added: 'SEND_MESSAGES', removed: [1]}},
				{key: 'color', old_value: Number.NaN, new_value: Number.POSITIVE_INFINITY},
				{key: 'hoist', old_value: null, new_value: null},
				{key: 'unicode_emoji', old_value: ['🔥'], new_value: ['⭐']},
				{key: 'position', old_value: {}, new_value: []},
			],
		});
		const result = presenter(entry);
		expect(() => resultToText(result)).not.toThrow();
		expect(result.rows).toEqual([]);
		expect(result.summary.values.role).toEqual({kind: 'role', id: '', recordedName: null});
	});
});
