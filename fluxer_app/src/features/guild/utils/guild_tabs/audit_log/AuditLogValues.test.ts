// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	makeEntry,
	TEST_ACTOR_ID,
	TEST_ENTRY_TIME,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogTestUtils';
import {
	actorPlaceholder,
	channelNoun,
	decodeAuditReason,
	diffBitfields,
	diffOverwriteStates,
	entryTime,
	featureDelta,
	largestDurationUnit,
	permissionFlagsIn,
	readBitfield,
	readBoolean,
	readChange,
	readNumber,
	readOption,
	readSnowflake,
	readString,
	readStringArray,
	readTimestamp,
	roleIdDiff,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ALL_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
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

const UNKNOWN_BIT = 1n << 62n;
const JUNK: Array<unknown> = [null, undefined, true, [], {}, Number.NaN];

describe('readString', () => {
	it('trims and keeps non-blank strings', () => {
		expect(readString(' general ')).toBe('general');
		expect(readString('0')).toBe('0');
	});

	it('treats blank strings and non-strings as missing', () => {
		for (const junk of ['', '   ', 5, ...JUNK]) {
			expect(readString(junk)).toBeNull();
		}
	});
});

describe('readNumber', () => {
	it('reads REST numbers and dispatch numeric strings', () => {
		expect(readNumber(3600)).toBe(3600);
		expect(readNumber(0)).toBe(0);
		expect(readNumber('3600')).toBe(3600);
		expect(readNumber(' 7 ')).toBe(7);
		expect(readNumber('-1.5')).toBe(-1.5);
	});

	it('rejects junk', () => {
		for (const junk of ['', '   ', 'x', '7 days', '0x10', Number.POSITIVE_INFINITY, ...JUNK]) {
			expect(readNumber(junk)).toBeNull();
		}
	});
});

describe('readBoolean', () => {
	it('reads REST booleans and dispatch boolean strings', () => {
		expect(readBoolean(true)).toBe(true);
		expect(readBoolean(false)).toBe(false);
		expect(readBoolean('true')).toBe(true);
		expect(readBoolean('false')).toBe(false);
	});

	it('rejects junk', () => {
		for (const junk of ['1', '0', 1, 0, 'yes', 'TRUE', '', null, undefined, {}]) {
			expect(readBoolean(junk)).toBeNull();
		}
	});
});

describe('readTimestamp', () => {
	it('parses date strings', () => {
		expect(readTimestamp('2026-09-01T12:00:00.000Z')).toBe(Date.parse('2026-09-01T12:00:00.000Z'));
		expect(readTimestamp('2026-09-01T12:00:00+00:00')).toBe(Date.parse('2026-09-01T12:00:00.000Z'));
	});

	it('rejects unparseable strings and non-strings', () => {
		for (const junk of ['not-a-date', '', '   ', 1788264000000, ...JUNK]) {
			expect(readTimestamp(junk)).toBeNull();
		}
	});
});

describe('readBitfield', () => {
	it('reads decimal strings and safe integers', () => {
		expect(readBitfield('8')).toBe(8n);
		expect(readBitfield('0')).toBe(0n);
		expect(readBitfield(8)).toBe(8n);
		expect(readBitfield((1n << 51n).toString())).toBe(Permissions.PIN_MESSAGES);
	});

	it('rejects junk', () => {
		for (const junk of ['-8', '8.5', ' 8', 'abc', '', 1.5, -1, 2 ** 53, 8n, ...JUNK]) {
			expect(readBitfield(junk)).toBeNull();
		}
	});
});

describe('readSnowflake', () => {
	it('reads snowflake strings', () => {
		expect(readSnowflake('1400000000000000123')).toBe('1400000000000000123');
	});

	it('rejects junk', () => {
		for (const junk of ['', 'abc', '12a', ' 123', '123456789012345678901', 1400000000000000000, ...JUNK]) {
			expect(readSnowflake(junk)).toBeNull();
		}
	});
});

describe('readStringArray', () => {
	it('reads arrays of strings', () => {
		expect(readStringArray(['a', 'b'])).toEqual(['a', 'b']);
		expect(readStringArray([])).toEqual([]);
	});

	it('rejects arrays holding anything but strings, and non-arrays', () => {
		for (const junk of [['a', 1], [null], 'a', ...JUNK.filter((value) => !Array.isArray(value))]) {
			expect(readStringArray(junk)).toBeNull();
		}
	});
});

describe('readChange', () => {
	it('reports presence from the keys that exist, counting explicit nulls', () => {
		const entry = makeEntry({
			action_type: AuditLogActionType.MEMBER_UPDATE,
			changes: [
				{key: 'nick', new_value: 'Hampus'},
				{key: 'avatar_hash', old_value: 'abc'},
				{key: 'banner_hash', old_value: null, new_value: null},
			],
		});
		expect(readChange(entry, 'nick')).toEqual({hasOld: false, hasNew: true, oldValue: null, newValue: 'Hampus'});
		expect(readChange(entry, 'avatar_hash')).toEqual({hasOld: true, hasNew: false, oldValue: 'abc', newValue: null});
		expect(readChange(entry, 'banner_hash')).toEqual({hasOld: true, hasNew: true, oldValue: null, newValue: null});
	});

	it('uses the first occurrence of a key', () => {
		const entry = makeEntry({
			action_type: AuditLogActionType.CHANNEL_UPDATE,
			changes: [
				{key: 'name', old_value: 'first-old', new_value: 'first-new'},
				{key: 'name', old_value: 'second-old', new_value: 'second-new'},
			],
		});
		expect(readChange(entry, 'name')).toEqual({
			hasOld: true,
			hasNew: true,
			oldValue: 'first-old',
			newValue: 'first-new',
		});
	});

	it('returns null for a missing key or missing changes', () => {
		const entry = makeEntry({action_type: AuditLogActionType.CHANNEL_UPDATE, changes: [{key: 'name', new_value: 'a'}]});
		expect(readChange(entry, 'topic')).toBeNull();
		expect(readChange(makeEntry({action_type: AuditLogActionType.MEMBER_KICK}), 'name')).toBeNull();
	});
});

describe('readOption', () => {
	it('returns REST typed values and raw dispatch strings as stored', () => {
		const rest = makeEntry({
			action_type: AuditLogActionType.INVITE_CREATE,
			options: {max_age: 604800, temporary: false, channel_id: '1400000000000000200'},
		});
		const dispatch = makeEntry({
			action_type: AuditLogActionType.INVITE_CREATE,
			options: {max_age: '604800', temporary: 'false'},
		});
		expect(readOption(rest, 'max_age')).toBe(604800);
		expect(readOption(rest, 'temporary')).toBe(false);
		expect(readOption(rest, 'channel_id')).toBe('1400000000000000200');
		expect(readNumber(readOption(dispatch, 'max_age'))).toBe(604800);
		expect(readBoolean(readOption(dispatch, 'temporary'))).toBe(false);
	});

	it('returns null for a missing key or missing options', () => {
		expect(readOption(makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD, options: {}}), 'count')).toBeNull();
		expect(readOption(makeEntry({action_type: AuditLogActionType.MEMBER_BAN_ADD}), 'count')).toBeNull();
	});
});

describe('entryTime', () => {
	it('reads the time from the entry snowflake', () => {
		expect(entryTime(makeEntry({action_type: AuditLogActionType.MEMBER_KICK}))).toBe(TEST_ENTRY_TIME);
	});
});

describe('actorPlaceholder', () => {
	it('is the acting user', () => {
		expect(actorPlaceholder(makeEntry({action_type: AuditLogActionType.MEMBER_KICK}))).toEqual({
			kind: 'user',
			id: TEST_ACTOR_ID,
		});
	});

	it('is the system when user_id is "0"', () => {
		expect(actorPlaceholder(makeEntry({action_type: AuditLogActionType.MEMBER_ROLE_UPDATE, user_id: '0'}))).toEqual({
			kind: 'system',
		});
	});
});

describe('decodeAuditReason', () => {
	it('keeps plain reasons, including a lone percent sign', () => {
		expect(decodeAuditReason('Spamming invites')).toBe('Spamming invites');
		expect(decodeAuditReason('Banned for 100% spam')).toBe('Banned for 100% spam');
	});

	it('decodes percent-encoded UTF-8', () => {
		expect(decodeAuditReason('Spamming%20invites')).toBe('Spamming invites');
		expect(decodeAuditReason('Caf%C3%A9%20raid%20%E7%A6%81%E6%AD%A2')).toBe('Café raid 禁止');
	});

	it('keeps a malformed sequence raw', () => {
		expect(decodeAuditReason('bad %E0%A4%A sequence')).toBe('bad %E0%A4%A sequence');
	});

	it('trims, and treats whitespace and missing reasons as null', () => {
		expect(decodeAuditReason('  padded  ')).toBe('padded');
		expect(decodeAuditReason('   ')).toBeNull();
		expect(decodeAuditReason('')).toBeNull();
		expect(decodeAuditReason('%20%20')).toBeNull();
		expect(decodeAuditReason(null)).toBeNull();
		expect(decodeAuditReason(undefined)).toBeNull();
	});
});

describe('largestDurationUnit', () => {
	it.each([
		[30, {unit: 'seconds', value: 30}],
		[90, {unit: 'seconds', value: 90}],
		[300, {unit: 'minutes', value: 5}],
		[3600, {unit: 'hours', value: 1}],
		[5400, {unit: 'minutes', value: 90}],
		[86400, {unit: 'days', value: 1}],
		[604800, {unit: 'days', value: 7}],
		[259199.993, {unit: 'days', value: 3}],
		[0, {unit: 'seconds', value: 0}],
	])('expresses %s seconds in the largest whole unit', (seconds, expected) => {
		expect(largestDurationUnit(seconds)).toEqual(expected);
	});
});

describe('permissionFlagsIn', () => {
	it('lists known flags in the Permissions declaration order', () => {
		expect(permissionFlagsIn(ALL_PERMISSIONS | UNKNOWN_BIT)).toEqual(Object.values(Permissions));
		expect(permissionFlagsIn(Permissions.MANAGE_ROLES | Permissions.KICK_MEMBERS | UNKNOWN_BIT)).toEqual([
			Permissions.KICK_MEMBERS,
			Permissions.MANAGE_ROLES,
		]);
		expect(permissionFlagsIn(0n)).toEqual([]);
	});
});

describe('diffBitfields', () => {
	it('lists added and removed known flags in declaration order', () => {
		expect(
			diffBitfields(
				Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES | UNKNOWN_BIT,
				Permissions.SEND_MESSAGES | Permissions.MANAGE_ROLES | Permissions.ADMINISTRATOR,
			),
		).toEqual({added: [Permissions.ADMINISTRATOR, Permissions.MANAGE_ROLES], removed: [Permissions.VIEW_CHANNEL]});
	});

	it('is empty for equal masks and unknown bits', () => {
		expect(diffBitfields(Permissions.SPEAK, Permissions.SPEAK)).toEqual({added: [], removed: []});
		expect(diffBitfields(0n, UNKNOWN_BIT)).toEqual({added: [], removed: []});
	});
});

describe('diffOverwriteStates', () => {
	it('reports an allow to deny flip as denied only', () => {
		expect(
			diffOverwriteStates({allow: Permissions.SEND_MESSAGES, deny: 0n}, {allow: 0n, deny: Permissions.SEND_MESSAGES}),
		).toEqual({allowed: [], denied: [Permissions.SEND_MESSAGES], cleared: []});
	});

	it('reports an allow that is no longer set as cleared', () => {
		expect(
			diffOverwriteStates(
				{allow: Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES, deny: 0n},
				{allow: Permissions.VIEW_CHANNEL, deny: 0n},
			),
		).toEqual({allowed: [], denied: [], cleared: [Permissions.SEND_MESSAGES]});
	});

	it('reports a deny that is no longer set as cleared', () => {
		expect(diffOverwriteStates({allow: 0n, deny: Permissions.ATTACH_FILES}, {allow: 0n, deny: 0n})).toEqual({
			allowed: [],
			denied: [],
			cleared: [Permissions.ATTACH_FILES],
		});
	});

	it('reports new allows and denies in declaration order', () => {
		expect(
			diffOverwriteStates(
				{allow: 0n, deny: 0n},
				{
					allow: Permissions.SPEAK | Permissions.VIEW_CHANNEL,
					deny: Permissions.MENTION_EVERYONE | Permissions.ADD_REACTIONS,
				},
			),
		).toEqual({
			allowed: [Permissions.VIEW_CHANNEL, Permissions.SPEAK],
			denied: [Permissions.ADD_REACTIONS, Permissions.MENTION_EVERYONE],
			cleared: [],
		});
	});

	it('ignores unknown bits', () => {
		expect(diffOverwriteStates({allow: UNKNOWN_BIT, deny: 1n << 61n}, {allow: 1n << 61n, deny: UNKNOWN_BIT})).toEqual({
			allowed: [],
			denied: [],
			cleared: [],
		});
	});
});

describe('roleIdDiff', () => {
	it('lists added ids in new order and removed ids in old order', () => {
		expect(roleIdDiff(['a', 'b', 'c', 'f'], ['e', 'c', 'd', 'a'])).toEqual({added: ['e', 'd'], removed: ['b', 'f']});
		expect(roleIdDiff(['x', 'b', 'y'], [])).toEqual({added: [], removed: ['x', 'b', 'y']});
	});

	it('lists each id once', () => {
		expect(roleIdDiff([], ['a', 'a'])).toEqual({added: ['a'], removed: []});
	});
});

describe('channelNoun', () => {
	it.each([
		[0, 'text'],
		[2, 'voice'],
		[998, 'link'],
		[4, 'category'],
		[1, 'generic'],
		[3, 'generic'],
		[null, 'generic'],
	] as const)('maps channel type %s to %s', (type, noun) => {
		expect(channelNoun(type)).toBe(noun);
	});
});

describe('featureDelta', () => {
	it('reports whether one feature was added or removed', () => {
		const feature = GuildFeatures.INVITES_DISABLED;
		expect(featureDelta([], [feature], feature)).toBe('added');
		expect(featureDelta([feature, 'OTHER'], ['OTHER'], feature)).toBe('removed');
		expect(featureDelta([feature], [feature], feature)).toBeNull();
		expect(featureDelta(['OTHER'], [], feature)).toBeNull();
	});
});
